use std::collections::VecDeque;
use std::fs::File;
use std::io;
use byteorder::{ReadBytesExt, BE};
use std::io::Read;

#[macro_use]
extern crate error_chain;
#[macro_use]
extern crate enum_kinds;

mod errors {
	error_chain! {
		foreign_links {
			Io(::std::io::Error);
		}
	}
}
use errors::*;

fn main() {
	let mut args: VecDeque<String> = std::env::args().collect();
	args.pop_front(); // skip our name
	let path = args.pop_front().expect("Must pass a file");
	let fi = File::open(path).expect("Cannot open file");
	read(RememberingBuffer::new(io::BufReader::new(fi))).expect("Failed to parse")
}

struct RememberingBuffer<T: io::Read> {
	pub reader: T,
	buf: Vec<u8>,
	offset: usize,
	column_width: usize,
	indent: i32
}
impl<T: io::Read> RememberingBuffer<T> {
	fn new(reader: T) -> RememberingBuffer<T> {
		RememberingBuffer{
			reader: reader,
			buf: Vec::new(),
			offset: 0,
			column_width: 16,
			indent: 0,
		}
	}

	fn last_reads(&mut self) -> Vec<u8> {
		let mut b = Vec::new();
		std::mem::swap(&mut self.buf, &mut b);
		b
	}

	fn explain_read<S: Into<String>>(&mut self, s: S) {
		let buf = self.last_reads();
		let descr_whole = s.into();
		let mut descr_lines = descr_whole.split("\n");
		let row = self.offset - (self.offset%self.column_width);
		let end_offset = self.offset + buf.len();
		let mut row_offsets = (row..end_offset).step_by(self.column_width);
		loop {
			match (row_offsets.next(), descr_lines.next()) {
				(None, None) => break,
				(Some(row_start), line) => {
					print!("{:<08X}: ", row_start);
					for idx in row_start..(row_start+self.column_width) {
						let mut byte = None;
						if idx >= self.offset {
							byte = buf.get(idx - self.offset);
						}
						match byte {
							Some(v) => print!("{:<02X} ", v),
							None => print!("   "),
						}
					}
					if let Some(l) = line {
						print!("| {:\t<1$}{2:}", "", self.indent as usize, l);
					}
					print!("\n");
				},
				(None, Some(line)) => {
					print!("{: <1$}| {2:\t<3$}{4:}\n", "", 8+2+(self.column_width*3),"", self.indent as usize, line);
				},
			}
		}
		self.offset = end_offset;
	}

	fn indent(&mut self, delta: i32) {
		self.indent += delta;
	}
}

impl <T: Read> io::Read for RememberingBuffer<T> {
	fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
		let r = self.reader.read(buf);
		if let Ok(size) = r {
			self.buf.extend(buf[0..size].iter())
		}
		r
	}
}

#[derive(Clone, Copy, Debug)]
struct CPIndex(u16);

impl std::fmt::Display for CPIndex {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match f.align() {
			Some(_) => write!(f, "CPI={: <4X}", self.0),
			None => write!(f, "CPI={:X}", self.0),
		}
	}
}

#[derive(Clone, Copy, Debug)]
struct BCIndex(u16);

impl std::fmt::Display for BCIndex {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		match f.align() {
			Some(_) => write!(f, "BCI={: <4X}", self.0),
			None => write!(f, "BCI={:X}", self.0),
		}
	}
}


impl std::ops::Index<CPIndex> for Vec<CPEntry> {
	type Output = CPEntry;
	fn index(&self, index: CPIndex) -> &Self::Output {
		&self[index.0 as usize]
	}
}
impl std::ops::IndexMut<CPIndex> for Vec<CPEntry> {
	fn index_mut(&mut self, index: CPIndex) -> &mut Self::Output {
		&mut self[index.0 as usize]
	}
}

#[derive(Clone, Debug, EnumKind)]
#[enum_kind(CPEntryKind)]
enum CPEntry {
	Unset,
	Class(CPIndex),
	Field(CPIndex, CPIndex),
	Method(CPIndex, CPIndex),
	InterfaceMethod(CPIndex, CPIndex),
	String(CPIndex),
	Integer(i32),
	Long(i64),
	Float(f32),
	Double(f64),
	NameAndType(CPIndex, CPIndex),
	Utf8(String),
}

struct CPLookup<'a>(&'a Vec<CPEntry>, CPEntryKind, CPIndex);
impl std::fmt::Display for CPLookup<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{:?} at {} containing ", self.1, self.2)?;
		let entry = &self.0[self.2];
		if let &CPEntry::Unset = entry {
			return Ok(());
		}
		let entry_type = CPEntryKind::from(entry);
		if entry_type != self.1 {
			write!(f, "incorrect type {:?} ", entry_type)
		} else {
			use CPEntry::*;
			match entry {
				&Unset => unreachable!(),
				&Class(name) => write!(f, "({})", CPLookup(self.0, CPEntryKind::Utf8, name)),
				&Field(class_index, nat_index) | &Method(class_index, nat_index) | &InterfaceMethod(class_index, nat_index) => 
					write!(f, "({}, {})",
						CPLookup(self.0, CPEntryKind::Class, class_index),
						CPLookup(self.0, CPEntryKind::NameAndType, nat_index)),
				&String(utf_index) => write!(f, "({})", CPLookup(self.0, CPEntryKind::Utf8, utf_index)),
				&Integer(v) => write!(f, "{}", v),
				&Long(v) => write!(f, "{}", v),
				&Float(v) => write!(f, "{}", v),
				&Double(v) => write!(f, "{}", v),
				&NameAndType(name_index, type_index) => write!(f, "({}, {})",
					CPLookup(self.0, CPEntryKind::Utf8, name_index),
					CPLookup(self.0, CPEntryKind::Utf8, type_index)),
				Utf8(s) => {
					if s.len() > 128 {
						write!(f, "{} (truncated)", &s[..128].escape_default())
					} else {
						write!(f, "{}", s.escape_default())
					}
				}
			}
		}
	}
}

struct ClassReader<T: io::Read> {
	fi: RememberingBuffer<T>,
	cp: Vec<CPEntry>,
}

#[derive(PartialEq)]
enum ObjectType {
	Class,
	Field,
	Method,
}

fn read<T: io::Read>(fi: RememberingBuffer<T>) -> Result<()> {
	let mut cl = ClassReader{fi: fi, cp: vec![CPEntry::Unset ;1]};
	cl.read_magic().chain_err(|| "magic")?;
	cl.read_version().chain_err(|| "header")?;
	cl.read_constant_pool().chain_err(|| "constant pool")?;
	cl.read_access_flags(ObjectType::Class).chain_err(|| "access flags")?;
	cl.read_inheritance().chain_err(|| "this & super class")?;
	cl.read_fields().chain_err(|| "fields")?;
	cl.read_methods().chain_err(|| "methods")?;
	{
		let num_attributes = cl.read_u16()?;
		for _ in 0..num_attributes {
			cl.read_attribute(ObjectType::Class).chain_err(|| "attributes")?;
		}
	}
	Ok(())
}

impl<T: io::Read> std::ops::Deref for ClassReader<T> {
	type Target = RememberingBuffer<T>;
	fn deref(&self) -> &Self::Target {
		&self.fi
	}
}
impl<T: io::Read> std::ops::DerefMut for ClassReader<T> {
	fn deref_mut(&mut self) -> &mut Self::Target {
		&mut self.fi
	}
}

impl<T: io::Read> ClassReader<T> {
	fn read_u16(&mut self) -> io::Result<u16> {
		self.fi.read_u16::<BE>()
	}
	fn read_cp(&mut self) -> io::Result<CPIndex> {
		Ok(CPIndex(self.fi.read_u16::<BE>()?))
	}
	fn read_bci(&mut self) -> io::Result<BCIndex> {
		Ok(BCIndex(self.fi.read_u16::<BE>()?))
	}
	fn read_u32(&mut self) -> io::Result<u32> {
		self.fi.read_u32::<BE>()
	}

	fn read_magic(&mut self) -> Result<()> {
		let magic = self.read_u32()?;
		self.explain_read("header");
		if magic != 0xCAFEBABE {
			panic!();
		}
		Ok(())
	}
	
	fn read_version(&mut self) -> Result<()> {
		let (minor, major) = (self.read_u16()?, self.read_u16()?);
		self.explain_read(&format!("Version {}.{}", major, minor));
		//TODO: matching jvm version
		Ok(())
	}
	
	fn read_constant_pool(&mut self) -> Result<()> {
		let cp_len = self.read_u16()?;
		self.cp = vec![CPEntry::Unset; cp_len as usize];
		self.explain_read("constant pool count");
		self.indent(1);
		let mut cp_index_int = 1;
		while cp_index_int < cp_len {
			let cp_index = CPIndex(cp_index_int);
			let tag = self.read_u8()?;
			match tag {
				7 => {
					let name_index = self.read_cp()?;
					self.fi.explain_read(format!("\
{:>}: CONSTANT_Class_info (JVMS§4.4.1)
	class name in {}", cp_index, CPLookup(&self.cp, CPEntryKind::Utf8, name_index)));
					self.cp[cp_index] = CPEntry::Class(name_index);
				},
				9|10|11 => {
					let class_index = self.read_cp()?;
					let nat_index = self.read_cp()?;
					let val = match tag {
						9 => CPEntry::Field(class_index, nat_index),
						10 => CPEntry::Method(class_index, nat_index),
						11 => CPEntry::InterfaceMethod(class_index, nat_index),
						_ => unreachable!(),
					};
					self.fi.explain_read(format!("\
{:>}: CONSTANT_{:?}ref_info (JVMS§4.4.2)
	class in {}
	signature in {}", cp_index, CPEntryKind::from(&val),
					CPLookup(&self.cp, CPEntryKind::Class, class_index),
					CPLookup(&self.cp, CPEntryKind::NameAndType, nat_index)));
					self.cp[cp_index] = val;
				},
				8 => {
					let string_index = self.read_cp()?;
					self.fi.explain_read(format!("\
{:>}: CONSTANT_String_info (JVMS§4.4.3)
	string in {}", cp_index, CPLookup(&self.cp, CPEntryKind::Utf8, string_index)));
					self.cp[cp_index] = CPEntry::String(string_index);
				},
				3 => {
					let val = self.read_i32::<BE>()?;
					self.explain_read(format!("\
{:>}: CONSTANT_Integer_info (JVMS§4.4.4)
	32 bit integer value {}", cp_index, val));
					self.cp[cp_index] = CPEntry::Integer(val);
				},
				4 => {
					let val = self.read_f32::<BE>()?;
					self.explain_read(format!("\
{:>}: CONSTANT_Float_info (JVMS§4.4.4)
	32 bit floating point value {}", cp_index, val));
					self.cp[cp_index] = CPEntry::Float(val);
				},
				5 => {
					let val = self.read_i64::<BE>()?;
					self.explain_read(format!("\
{:>}: CONSTANT_Long_info (JVMS§4.4.5)
	64 bit integer value {}", cp_index, val));
					self.cp[cp_index] = CPEntry::Long(val);
					cp_index_int+=1;
				},
				6 => {
					let val = self.read_f64::<BE>()?;
					self.explain_read(format!("\
{:>}: CONSTANT_Double_info (JVMS§4.4.5)
	64 bit floating point value {}", cp_index, val));
					self.cp[cp_index] = CPEntry::Double(val);
					cp_index_int+=1;
				},
				12 => {
					let name_index = self.read_cp()?;
					let descriptor_index = self.read_cp()?;
					self.fi.explain_read(format!("\
{:>}: CONSTANT_NameAndType_info (JVMS§4.4.6)
	name in {}
	descriptor in {}", cp_index,
	CPLookup(&self.cp, CPEntryKind::Utf8, name_index),
	CPLookup(&self.cp, CPEntryKind::Utf8, descriptor_index)));
					self.cp[cp_index] = CPEntry::NameAndType(name_index, descriptor_index);
				},
				1 => {
					let s = {
						let len = self.read_u16()?;
						let mut buf = vec![0; len as usize];
						self.read_exact(&mut buf)?;
						parse_modified_utf8(&buf).unwrap_or("invalid string".to_owned())
					};
					self.explain_read(format!("\
{:>}: CONSTANT_Utf8_info (JVMS§4.4.4)
	{}", cp_index, s.escape_default()));
					self.cp[cp_index] = CPEntry::Utf8(s);
				},
				//TODO: java8 stuff
				_ => {
					panic!("{:>}: Unknown tag type {}", cp_index, tag);
				},
			}
			cp_index_int+=1;
		}
		self.indent(-1);
		Ok(())
	}

	fn read_access_flags(&mut self, typ: ObjectType) -> Result<()> {
		let flags = self.read_u16()?;
		use ObjectType::*;
		let section = match typ {
			Class => "4.1-A",
			Field => "4.5-A",
			Method => "4.6-A",
		};
		let mut s = String::from(format!("access_flags (JVMS§{})", section));
		let flag_names = [
			(0x0001, "ACC_PUBLIC", true, true, true),
			(0x0002, "ACC_PRIVATE", false, true, true),
			(0x0004, "ACC_PROTECTED", false, true, true),
			(0x0008, "ACC_STATIC", false, true, true),
			(0x0010, "ACC_FINAL", true, true, true),
			(0x0020, "ACC_SUPER", true, false, false),
			(0x0020, "ACC_SYNCHRONIZED", false, false, true),
			(0x0040, "ACC_VOLATILE", false, true, false),
			(0x0040, "ACC_BRIDGE", false, false, true),
			(0x0080, "ACC_TRANSIENT", false, true, false),
			(0x0080, "ACC_VARARGS", false, false, true),
			(0x0100, "ACC_NATIVE", false, false, true),
			(0x0200, "ACC_INTERFACE", true, false, false),
			(0x0400, "ACC_ABSTRACT", true, false, true),
			(0x1000, "ACC_SYNTHETIC", true, true, true),
			(0x2000, "ACC_ANNOTATION", true, false, false),
			(0x4000, "ACC_ENUM", true, true, false),
		];
		for bit in 0..16 {
			let mask = 1u16 << bit;
			if (flags & mask) != 0 {
				let flags = flag_names.iter()
					.filter(|x| x.0 == mask)
					.copied()
					.collect::<Vec<_>>();
				let flag = flags.iter().find(|flag| match typ {
					Class => flag.2,
					Field => flag.3,
					Method => flag.4,
				});
				let text = match flag {
					Some(f) => f.1,
					None => match flags.len() {
						0 => "invalid flag",
						_ => "invalid flag for type",
					},
				};
				s = format!("{}\n\t{:<04X} {}", s, mask, text);
			}
		}
		self.explain_read(s);
		Ok(())
	}

	fn read_inheritance(&mut self) -> Result<()> {
		let this_class = self.read_cp()?;
		self.fi.explain_read(format!("\
this_class (JVMS§4.1)
	class info in {}", CPLookup(&self.cp, CPEntryKind::Class, this_class)));

	let super_class = self.read_cp()?;
	self.fi.explain_read(format!("\
super_class (JVMS§4.1)
	superclass info in {}", CPLookup(&self.cp, CPEntryKind::Class, super_class)));

		let num_interfaces = self.read_u16()?;
		self.explain_read("number of interfaces");
		self.indent(1);
		for _ in 0..num_interfaces {
			let iface =  self.read_cp()?;
			self.fi.explain_read(format!("\
implements {}", CPLookup(&self.cp, CPEntryKind::Class, iface)));
		}
		self.indent(-1);
		Ok(())
	}

	fn read_fields(&mut self) -> Result<()> {
		let num_fields = self.read_u16()?;
		self.explain_read("number of fields");
		self.indent(1);
		for _ in 0..num_fields {
			self.explain_read("field_info (JVMS§4.5)");
			self.indent(1);
			self.read_access_flags(ObjectType::Field)?;
			let name_index = self.read_cp()?;
			self.fi.explain_read(format!("name in {}", CPLookup(&self.cp, CPEntryKind::Utf8, name_index)));
			let descriptor_index = self.read_cp()?;
			self.fi.explain_read(format!("descriptor in {}", CPLookup(&self.cp, CPEntryKind::Utf8,descriptor_index)));
			let num_attributes = self.read_u16()?;
			self.explain_read(format!("{} attributes", num_attributes));
			self.indent(1);
			for _ in 0..num_attributes {
				self.read_attribute(ObjectType::Field)?;
			}
			self.indent(-2);
		}
		self.indent(-1);
		Ok(())
	}

	fn read_methods(&mut self) -> Result<()> {
		let num_methods = self.read_u16()?;
		self.explain_read("number of methods");
		self.indent(1);
		for _ in 0..num_methods {
			self.explain_read("method_info (JVMS§4.6)");
			self.indent(1);
			self.read_access_flags(ObjectType::Method)?;
			let name_index = self.read_cp()?;
			self.fi.explain_read(format!("name in {}", CPLookup(&self.cp, CPEntryKind::Utf8, name_index)));
			let descriptor_index = self.read_cp()?;
			self.fi.explain_read(format!("descriptor in {}", CPLookup(&self.cp, CPEntryKind::Utf8, descriptor_index)));
			let num_attributes = self.read_u16()?;
			self.explain_read(format!("{} attributes", num_attributes));
			self.indent(1);
			for _ in 0..num_attributes {
				self.read_attribute(ObjectType::Method)?;
			}
			self.indent(-2);
		}
		self.indent(-1);
		Ok(())
	}

	fn read_attribute(&mut self, typ: ObjectType) -> Result<()> {
		let name_index = self.read_cp()?;
		let length = self.read_u32()?;
		let iname = if let CPEntry::Utf8(name) = &self.cp[name_index] {
			name.as_ref()
		} else {
			""
		};
		match (iname, typ) {
			("Code", ObjectType::Method) => self.read_code()?,
			("SourceFile", ObjectType::Class) => {
				self.explain_read("SourceFile attribute (JVMS§4.7.10)");
				let sourcefile_index = self.read_cp()?;
				self.fi.explain_read(format!("sourcefile in {}", CPLookup(&self.cp, CPEntryKind::Utf8, sourcefile_index)));
			},
			_ => {
				let mut buf = vec![0; length as usize];
				self.read_exact(&mut buf)?;
				self.fi.explain_read(format!("attribute with name ({}) of length {}", CPLookup(&self.cp, CPEntryKind::Utf8, name_index), length))
			},
		}
		Ok(())
	}

	fn read_code(&mut self) -> Result<()> {
		self.explain_read("Code attribute (JVMS§4.7.3)");
		self.indent(1);
		{
			let max_stack = self.read_u16()?;
			self.explain_read(format!("maximum stack depth {}", max_stack));
		}
		{
			let max_locals = self.read_u16()?;
			self.explain_read(format!("local variable table size {}", max_locals));
		}
		{
			let code_length = self.read_u32()?;
			self.explain_read(format!("method bytecode length {}", code_length));
			let mut buf = vec![0; code_length as usize];
			self.read_exact(&mut buf)?;
			self.indent(1);
			self.explain_read("method bytecode");
			self.indent(-1);
		}
		{
			let exception_table_length = self.read_u16()?;
			self.explain_read(format!("exception table of length {} (JVMS§4.7.3, JVMS§2.10)", exception_table_length));
			self.indent(1);
			for i in 0..exception_table_length {
				let start_pc = self.read_bci()?;
				let end_pc = self.read_bci()?;
				let handler_pc = self.read_bci()?;
				let catch_type = self.read_cp()?;
				self.fi.explain_read(format!("\
exception handler {} from {} to {}. handler at {}; catches {}",
					i, start_pc, end_pc, handler_pc, CPLookup(&self.cp, CPEntryKind::Class, catch_type)));
			}
			self.indent(-1);
		}
		{
			let num_attributes = self.read_u16()?;
			self.explain_read(format!("{} attributes", num_attributes));
			self.indent(1);
			for _ in 0..num_attributes {
				self.read_attribute(ObjectType::Method)?;
			}
		}
		self.indent(-2);
		Ok(())
	}
}

fn parse_modified_utf8(b: &[u8]) -> Option<String> {
	let mut buf: Vec<u16> = Vec::new();
	let mut u8r = b.iter().copied();
	loop {
		let u8c = match u8r.next() {
			Some(v) => v,
			None => break,
		};
		let mut c = u8c as u16;
		if u8c >> 7 == 0b1 {
			c = c & 0b0011_1111;
			if u8c >> 5 == 0b111 {
				c = c & 0b0001_1111;
				if u8c >> 4 == 0b1111 {
					c = c & 0b0000_1111;
					c = c << 6 | (u8r.next()? & 0b0011_1111) as u16;
				}
				c = c << 6 | (u8r.next()? & 0b0011_1111) as u16;
			}
			c = c << 6 | (u8r.next()? & 0b0011_1111) as u16;
		}
		buf.push(c);
	}
	Some(std::string::String::from_utf16_lossy(&buf))
}