use std::collections::VecDeque;
use std::fs::File;
use std::io;
use byteorder::{ReadBytesExt, BE};
use std::io::Read;

#[macro_use]
extern crate error_chain;

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
	pub offset: usize,
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
		if self.indent < 0 {
			panic!("mismatched indent");
		}
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

#[derive(Clone, Debug)]
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum CPEntryKind {
	AnyMethod,
	AnyConstant1,
	AnyConstant2,
	Unset,
	Class,
	Field,
	Method,
	InterfaceMethod,
	String,
	Integer,
	Long,
	Float,
	Double,
	NameAndType,
	Utf8,
}

impl From<&CPEntry> for CPEntryKind {
	fn from(e: &CPEntry) -> Self {
		match e {
			&CPEntry::Unset => CPEntryKind::Unset,
			&CPEntry::Class(_) => CPEntryKind::Class,
			&CPEntry::Field(_, _)=> CPEntryKind::Field,
			&CPEntry::Method(_, _)=> CPEntryKind::Method,
			&CPEntry::InterfaceMethod(_, _)=> CPEntryKind::InterfaceMethod,
			&CPEntry::String(_) => CPEntryKind::String,
			&CPEntry::Integer(_)=> CPEntryKind::Integer,
			&CPEntry::Long(_)=> CPEntryKind::Long,
			&CPEntry::Float(_) => CPEntryKind::Float,
			&CPEntry::Double(_) => CPEntryKind::Double,
			&CPEntry::NameAndType(_, _) => CPEntryKind::NameAndType,
			&CPEntry::Utf8(_) => CPEntryKind::Utf8,
		}
	}
}

impl CPEntryKind {
	fn is(self, other: CPEntryKind) -> bool {
		match self {
			s if s == other => true,
			CPEntryKind::AnyMethod => other == CPEntryKind::Method || other == CPEntryKind::InterfaceMethod,
			CPEntryKind::AnyConstant1 => [
				CPEntryKind::Integer,
				CPEntryKind::Float,
				CPEntryKind::Class,
				CPEntryKind::String,
				//CPEntryKind::MethodHandle,
				//CPEntryKind::MethodType,
				//CPEntryKind::Dynamic,
			].contains(&other),
			CPEntryKind::AnyConstant2 => [
				CPEntryKind::Long,
				CPEntryKind::Double,
				//CPEntryKind::Dynamic,
			].contains(&other),
			_ => false,
		}
	}
}

struct CPLookup<'a>(&'a Vec<CPEntry>, CPEntryKind, CPIndex);
impl std::fmt::Display for CPLookup<'_> {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let entry = &self.0[self.2];
		if let &CPEntry::Unset = entry {
			write!(f, "{:?} at {} containing ", self.1, self.2)?;
			return Ok(());
		}
		let entry_type = CPEntryKind::from(entry);
		if !self.1.is(entry_type) {
			write!(f, "{:?} at {} containing ", self.1, self.2)?;
			write!(f, "incorrect type {:?} ", entry_type)
		} else {
			write!(f, "{:?} at {} containing ", entry_type, self.2)?;
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
	fn read_i16(&mut self) -> io::Result<i16> {
		self.fi.read_i16::<BE>()
	}
	fn read_cp8(&mut self) -> io::Result<CPIndex> {
		Ok(CPIndex(self.fi.read_u8()? as u16))
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
	fn read_i32(&mut self) -> io::Result<i32> {
		self.fi.read_i32::<BE>()
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
					let val = self.read_i32()?;
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
				self.indent(1);
				let sourcefile_index = self.read_cp()?;
				self.fi.explain_read(format!("sourcefile in {}", CPLookup(&self.cp, CPEntryKind::Utf8, sourcefile_index)));
				self.indent(-1);
			},
			("RuntimeVisibleAnnotations", _) => {
				self.explain_read("RuntimeVisibleAnnotations attribute (JVMS§4.7.16)");
				self.indent(1);
				let num_annotations = self.read_u16()?;
				self.fi.explain_read(format!("{} annotations", num_annotations));
				for _ in 0..num_annotations {
					self.read_annotation()?;
				}
				self.indent(-1);
			},
			("RuntimeInvisibleAnnotations", _) => {
				self.explain_read("RuntimeInvisibleAnnotations attribute (JVMS§4.7.17)");
				self.indent(1);
				let num_annotations = self.read_u16()?;
				self.fi.explain_read(format!("{} annotations", num_annotations));
				for _ in 0..num_annotations {
					self.read_annotation()?;
				}
				self.indent(-1);
			},
			_ => {
				let mut buf = vec![0; length as usize];
				self.read_exact(&mut buf)?;
				self.fi.explain_read(format!("attribute with name ({}) of length {}", CPLookup(&self.cp, CPEntryKind::Utf8, name_index), length))
			},
		}
		Ok(())
	}

	fn read_annotation(&mut self) -> Result<()> {
		let type_name_index = self.read_cp()?;
		let num_element_value_pairs = self.read_u16()?;
		self.fi.explain_read(format!("annotation type ({}) with {} elements",
			CPLookup(&self.cp, CPEntryKind::Utf8, type_name_index), num_element_value_pairs));
		self.indent(1);
		for _ in 0..num_element_value_pairs {
			let element_name_index = self.read_cp()?;
			self.fi.explain_read(format!("element with name ({}) (JVMS§4.7.16.1)", CPLookup(&self.cp, CPEntryKind::Utf8, element_name_index)));
			self.read_annotation_element()?;
		}
		self.indent(-1);
		Ok(())
	}

	fn read_annotation_element(&mut self) -> Result<()> {
		self.indent(1);
		let tag = self.read_u8()?;
		match tag {
			b'e' => {
				let type_name_index = self.read_cp()?;
				let const_name_index = self.read_cp()?;
				self.fi.explain_read(format!("enum type ({}) with value ({})",
					CPLookup(&self.cp, CPEntryKind::Utf8, type_name_index),
					CPLookup(&self.cp, CPEntryKind::Utf8, const_name_index)));
			},
			b'c' => {
				let class_info_index = self.read_cp()?;
				self.fi.explain_read(format!("class type ({})",
					CPLookup(&self.cp, CPEntryKind::Utf8, class_info_index)));
			},
			b'@' => {
				self.read_annotation()?;
			},
			b'[' => {
				let num_values = self.read_u16()?;
				self.explain_read(format!("array of {} values", num_values));
				for _ in 0..num_values {
					self.read_annotation_element()?;
				}
			},
			_ => {
				let (name, kind) = match tag {
					b's' => ("String", CPEntryKind::String),
					b'B' => ("byte", CPEntryKind::Integer),
					b'C' => ("char", CPEntryKind::Integer),
					b'S' => ("short", CPEntryKind::Integer),
					b'I' => ("int", CPEntryKind::Integer),
					b'F' => ("float", CPEntryKind::Float),
					b'D' => ("double", CPEntryKind::Double),
					b'J' => ("long", CPEntryKind::Long),
					b'Z' => ("boolean", CPEntryKind::Integer),
					_ => panic!("Unknown tag type {}", tag),
				};
				let const_value_index = self.read_cp()?;
				self.fi.explain_read(format!("{} with value ({})", name, CPLookup(&self.cp, kind, const_value_index)));
			},
		}
		self.indent(-1);
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
			self.indent(1);
			self.read_bytecode(self.offset + code_length as usize)?;
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

	fn read_maybewide(&mut self, is_wide: bool) -> Result<u16> {
		if is_wide {
			Ok(self.read_u16()?)
		} else {
			Ok(self.read_u8()? as u16)
		}
	}

	fn explain_branch(&mut self, name: &'static str) -> Result<()> {
		let offset = self.read_i16()? as i32;
		let ins_offset =  self.offset as i32;
		self.explain_read(format!("{} 0x{:>} (JVMS§6.5)", name, ins_offset + offset));
		Ok(())
	}

	fn read_bytecode(&mut self, end: usize) -> Result<()> {
		let start = self.offset;
		let mut is_wide = false;
		while self.offset < end {
			match self.read_u8()? {
				0x32 => self.explain_read("aaload (JVMS§6.5)"),
				0x53 => self.explain_read("aastore (JVMS§6.5)"),
				0x01 => self.explain_read("aconst_null (JVMS§6.5)"),
				0x19 => {
					let index = self.read_maybewide(is_wide)?;
					self.explain_read(format!("aload {} (JVMS§6.5)", index));
				},
				index if index >= 0x2a && index <= 0x2d => self.explain_read(format!("aload_{} (JVMS§6.5)", index - 0x2a)),
				0xbd => {
					let typ = self.read_cp()?;
					self.fi.explain_read(format!("anewarray {} (JVMS§6.5)", CPLookup(&self.cp, CPEntryKind::Class, typ)));
				},
				0xb0 => self.explain_read("areturn (JVMS§6.5)"),
				0xbe => self.explain_read("arraylength (JVMS§6.5)"),
				0x3a => {
					let index = self.read_maybewide(is_wide)?;
					self.explain_read(format!("areturn {} (JVMS§6.5)", index));
				},
				index if index >= 0x4b && index <= 0x4e => self.explain_read(format!("astore_{} (JVMS§6.5)", index - 0x4b)),
				0xbf => self.explain_read("athrow (JVMS§6.5)"),
				0x33 => self.explain_read("baload (JVMS§6.5)"),
				0x54 => self.explain_read("bastore (JVMS§6.5)"),
				0x10 => {
					let value = self.read_i8()? as i32;
					self.explain_read(format!("bipush {} (JVMS§6.5)", value))
				},
				0x34 => self.explain_read("caload (JVMS§6.5)"),
				0x55 => self.explain_read("castore (JVMS§6.5)"),
				0xc0 => {
					let index = self.read_maybewide(is_wide)?;
					self.explain_read(format!("checkcast {} (JVMS§6.5)", index));
				},
				0x90 => self.explain_read("d2f (JVMS§6.5)"),
				0x8e => self.explain_read("d2i (JVMS§6.5)"),
				0x8f => self.explain_read("d2l (JVMS§6.5)"),
				0x63 => self.explain_read("dadd (JVMS§6.5)"),
				0x31 => self.explain_read("daload (JVMS§6.5)"),
				0x52 => self.explain_read("dastore (JVMS§6.5)"),
				0x98 => self.explain_read("dcmpg (JVMS§6.5)"),
				0x97 => self.explain_read("dcmpl (JVMS§6.5)"),
				0x0e => self.explain_read("dconst_0 (JVMS§6.5)"),
				0x0f => self.explain_read("dconst_1 (JVMS§6.5)"),
				0x6f => self.explain_read("ddiv (JVMS§6.5)"),
				0x18 => {
					let index = self.read_maybewide(is_wide)?;
					self.explain_read(format!("dload {} (JVMS§6.5)", index));
				},
				index if index >= 0x26 && index <= 0x29 => self.explain_read(format!("dload_{} (JVMS§6.5)", index - 0x26)),
				0x6b => self.explain_read("dmul (JVMS§6.5)"),
				0x77 => self.explain_read("dneg (JVMS§6.5)"),
				0x73 => self.explain_read("drem (JVMS§6.5)"),
				0xaf => self.explain_read("dreturn (JVMS§6.5)"),
				0x39 => {
					let index = self.read_maybewide(is_wide)?;
					self.explain_read(format!("dstore {} (JVMS§6.5)", index));
				},
				index if index >= 0x47 && index <= 0x4a => self.explain_read(format!("dstore_{} (JVMS§6.5)", index - 0x47)),
				0x67 => self.explain_read("dsub (JVMS§6.5)"),
				0x59 => self.explain_read("dup (JVMS§6.5)"),
				0x5a => self.explain_read("dup_x1 (JVMS§6.5)"),
				0x5b => self.explain_read("dup_x2 (JVMS§6.5)"),
				0x5c => self.explain_read("dup2 (JVMS§6.5)"),
				0x5d => self.explain_read("dup2_x1 (JVMS§6.5)"),
				0x5e => self.explain_read("dup2_x2 (JVMS§6.5)"),
				0x8d => self.explain_read("f2d (JVMS§6.5)"),
				0x8b => self.explain_read("f2i (JVMS§6.5)"),
				0x8c => self.explain_read("f2l (JVMS§6.5)"),
				0x62 => self.explain_read("fadd (JVMS§6.5)"),
				0x30 => self.explain_read("faload (JVMS§6.5)"),
				0x51 => self.explain_read("fastore (JVMS§6.5)"),
				0x96 => self.explain_read("fcmpg (JVMS§6.5)"),
				0x95 => self.explain_read("fcmpl (JVMS§6.5)"),
				v if v >= 0x0b && v <= 0x0d => self.explain_read(format!("fconst_{} (JVMS§6.5)", v - 0x0b)),
				0x6e => self.explain_read("fdiv (JVMS§6.5)"),
				0x17 => {
					let index = self.read_maybewide(is_wide)?;
					self.explain_read(format!("fload {} (JVMS§6.5)", index));
				},
				index if index >= 0x22 && index <= 0x25 => self.explain_read(format!("fload_{} (JVMS§6.5)", index - 0x22)),
				0x6a => self.explain_read("fmul (JVMS§6.5)"),
				0x76 => self.explain_read("fneg (JVMS§6.5)"),
				0x72 => self.explain_read("frem (JVMS§6.5)"),
				0xae => self.explain_read("freturn (JVMS§6.5)"),
				0x38 => {
					let index = self.read_maybewide(is_wide)?;
					self.explain_read(format!("fstore {} (JVMS§6.5)", index));
				},
				index if index >= 0x43 && index <= 0x46 => self.explain_read(format!("fload_{} (JVMS§6.5)", index - 0x43)),
				0x66 => self.explain_read("fsub (JVMS§6.5)"),
				0xb4 => {
					let typ = self.read_cp()?;
					self.fi.explain_read(format!("getfield {} (JVMS§6.5)", CPLookup(&self.cp, CPEntryKind::Field, typ)));
				},
				0xb2 => {
					let typ = self.read_cp()?;
					self.fi.explain_read(format!("getstatic {} (JVMS§6.5)", CPLookup(&self.cp, CPEntryKind::Field, typ)));
				},
				0xa7 => self.explain_branch("goto")?,
				0xc8 => {
					let offset = self.read_i32()?;
					self.fi.explain_read(format!("goto_w 0x{:>} (JVMS§6.5)", self.offset as i32 + offset))
				},
				0x91 => self.explain_read("i2b (JVMS§6.5)"),
				0x92 => self.explain_read("i2c (JVMS§6.5)"),
				0x87 => self.explain_read("i2d (JVMS§6.5)"),
				0x86 => self.explain_read("i2f (JVMS§6.5)"),
				0x85 => self.explain_read("i2l (JVMS§6.5)"),
				0x93 => self.explain_read("i2s (JVMS§6.5)"),
				0x60 => self.explain_read("iadd (JVMS§6.5)"),
				0x2e => self.explain_read("iaload (JVMS§6.5)"),
				0x7e => self.explain_read("iand (JVMS§6.5)"),
				0x4f => self.explain_read("iastore (JVMS§6.5)"),
				0x02 => self.explain_read("iconst_m1 (JVMS§6.5)"),
				v if v >= 0x03 && v <= 0x08 => self.explain_read(format!("iconst_{} (JVMS§6.5)", v - 0x03)),
				0x6c => self.explain_read("idiv (JVMS§6.5)"),
				0xa5 => self.explain_branch("if_acmpeq")?,
				0xa6 => self.explain_branch("if_acmpne")?,
				0x9f => self.explain_branch("if_icmpeq")?,
				0xa0 => self.explain_branch("if_icmpne")?,
				0xa1 => self.explain_branch("if_icmplt")?,
				0xa2 => self.explain_branch("if_icmpge")?,
				0xa3 => self.explain_branch("if_icmpgt")?,
				0xa4 => self.explain_branch("if_icmple")?,
				0x99 => self.explain_branch("ifeq")?,
				0x9a => self.explain_branch("ifne")?,
				0x9b => self.explain_branch("iflt")?,
				0x9c => self.explain_branch("ifge")?,
				0x9d => self.explain_branch("ifgt")?,
				0x9e => self.explain_branch("ifle")?,
				0xc7 => self.explain_branch("ifnonnull")?,
				0xc6 => self.explain_branch("ifnull")?,
				0x84 => {
					let index = self.read_maybewide(is_wide)?;
					let value = if is_wide { self.read_i16()? } else { self.read_i8()? as i16 };
					self.explain_read(format!("iinc {} += {} (JVMS§6.5)", index, value));
				},
				0x15 => {
					let index = self.read_maybewide(is_wide)?;
					self.explain_read(format!("iload {} (JVMS§6.5)", index));
				},
				index if index >= 0x1a && index <= 0x1d => self.explain_read(format!("iload_{} (JVMS§6.5)", index - 0x1a)),
				0x68 => self.explain_read("imul (JVMS§6.5)"),
				0x74 => self.explain_read("ineg (JVMS§6.5)"),
				0xc1 => {
					let typ = self.read_cp()?;
					self.fi.explain_read(format!("instanceof {} (JVMS§6.5)", CPLookup(&self.cp, CPEntryKind::Class, typ)));
				},
				/*0xba => {
					let typ = self.read_cp()?;
					let zeros = self.read_u16()?;
					if zeros != 0 {
						panic!("invalid invokedynamic");
					}
					self.explain_read(format!("invokedynamic {} (JVMS§6.5)", CPLookup(&self.cp, CPEntryKind::InvokeDynamicInfo, typ)));
				},*/
				0xb9 => {
					let typ = self.read_cp()?;
					let _arg_count = self.read_u8()?; // probably should check this idc
					let zero = self.read_u8()?;
					if zero != 0 {
						panic!("invalid invokeinterface");
					}
					self.fi.explain_read(format!("invokeinterface {} (JVMS§6.5)", CPLookup(&self.cp, CPEntryKind::InterfaceMethod, typ)));
				},
				0xb7 => {
					let typ = self.read_cp()?;
					self.fi.explain_read(format!("invokespecial {} (JVMS§6.5)", CPLookup(&self.cp, CPEntryKind::AnyMethod, typ)));
				},
				0xb8 => {
					let typ = self.read_cp()?;
					self.fi.explain_read(format!("invokestatic {} (JVMS§6.5)", CPLookup(&self.cp, CPEntryKind::AnyMethod, typ)));
				},
				0xb6 => {
					let typ = self.read_cp()?;
					self.fi.explain_read(format!("invokevirtual {} (JVMS§6.5)", CPLookup(&self.cp, CPEntryKind::Method, typ)));
				},
				0x80 => self.explain_read("ior (JVMS§6.5)"),
				0x70 => self.explain_read("irem (JVMS§6.5)"),
				0xac => self.explain_read("ireturn (JVMS§6.5)"),
				0x78 => self.explain_read("ishl (JVMS§6.5)"),
				0x7a => self.explain_read("ishr (JVMS§6.5)"),
				0x36 => {
					let index = self.read_maybewide(is_wide)?;
					self.explain_read(format!("istore {} (JVMS§6.5)", index));
				},
				index if index >= 0x3b && index <= 0x3e => self.explain_read(format!("istore_{} (JVMS§6.5)", index - 0x3b)),
				0x64 => self.explain_read("isub (JVMS§6.5)"),
				0x7c => self.explain_read("iushr (JVMS§6.5)"),
				0x82 => self.explain_read("ixor (JVMS§6.5)"),
				0xa8 => self.explain_branch("jsr")?,
				0xc9 => {
					let offset = self.read_i32()?;
					self.fi.explain_read(format!("jsr_w 0x{:>} (JVMS§6.5)", self.offset as i32 + offset))
				},
				0x8a => self.explain_read("l2d (JVMS§6.5)"),
				0x89 => self.explain_read("l2f (JVMS§6.5)"),
				0x88 => self.explain_read("l2i (JVMS§6.5)"),
				0x61 => self.explain_read("ladd (JVMS§6.5)"),
				0x2f => self.explain_read("laload (JVMS§6.5)"),
				0x7f => self.explain_read("land (JVMS§6.5)"),
				0x50 => self.explain_read("lastore (JVMS§6.5)"),
				0x94 => self.explain_read("lcmp (JVMS§6.5)"),
				0x09 => self.explain_read("lconst_0 (JVMS§6.5)"),
				0x0a => self.explain_read("lconst_1 (JVMS§6.5)"),
				0x12 => {
					let typ = self.read_cp8()?;
					self.fi.explain_read(format!("ldc {} (JVMS§6.5)", CPLookup(&self.cp, CPEntryKind::AnyConstant1, typ)));
				},
				0x13 => {
					let typ = self.read_cp()?;
					self.fi.explain_read(format!("ldc_w {} (JVMS§6.5)", CPLookup(&self.cp, CPEntryKind::AnyConstant1, typ)));
				},
				0x14 => {
					let typ = self.read_cp()?;
					self.fi.explain_read(format!("ldc2_w {} (JVMS§6.5)", CPLookup(&self.cp, CPEntryKind::AnyConstant2, typ)));
				},
				0x6d => self.explain_read("ldiv (JVMS§6.5)"),
				0x16 => {
					let index = self.read_maybewide(is_wide)?;
					self.explain_read(format!("lload {} (JVMS§6.5)", index));
				},
				index if index >= 0x1e && index <= 0x21 => self.explain_read(format!("lload_{} (JVMS§6.5)", index - 0x1e)),
				0x69 => self.explain_read("lmul (JVMS§6.5)"),
				0x75 => self.explain_read("lneg (JVMS§6.5)"),
				0xab => {
					let bc_offset = self.offset as i32;
					self.explain_read("lookupswitch (JVMS§6.5)");
					self.indent(1);
					let padding = (self.offset - start) % 4;
					if padding > 0 {
						self.read_exact(&mut vec![0; 4 - padding])?;
						self.explain_read("padding")
					}
					let default = bc_offset + self.read_i32()?;
					self.explain_read(format!("default branch {:>}", default));
					let npairs = self.read_i32()?;
					self.explain_read("number of cases");
					self.indent(1);
					for _ in 0..npairs {
						let key = self.read_i32()?;
						let branch = bc_offset + self.read_i32()?;
						self.explain_read(format!("case {}: branch {:>}", key, branch));
					}
					self.indent(-2);
				},
				0x81 => self.explain_read("lor (JVMS§6.5)"),
				0x71 => self.explain_read("lrem (JVMS§6.5)"),
				0xad => self.explain_read("lreturn (JVMS§6.5)"),
				0x79 => self.explain_read("lshl (JVMS§6.5)"),
				0x7b => self.explain_read("lshr (JVMS§6.5)"),
				0x37 => {
					let index = self.read_maybewide(is_wide)?;
					self.explain_read(format!("lstore {} (JVMS§6.5)", index));
				},
				index if index >= 0x3f && index <= 0x42 => self.explain_read(format!("lstore_{} (JVMS§6.5)", index - 0x3f)),
				0x65 => self.explain_read("lsub (JVMS§6.5)"),
				0x7d => self.explain_read("lushr (JVMS§6.5)"),
				0x83 => self.explain_read("lxor (JVMS§6.5)"),
				0xc2 => self.explain_read("monitorenter (JVMS§6.5)"),
				0xc3 => self.explain_read("monitorexit (JVMS§6.5)"),
				0xc5 => {
					let typ = self.read_cp()?;
					let dims = self.read_u8()?;
					self.fi.explain_read(format!("multianewarray {} dimensions {} (JVMS§6.5)", CPLookup(&self.cp, CPEntryKind::Class, typ), dims));
				},
				0xbb => {
					let typ = self.read_cp()?;
					self.fi.explain_read(format!("new {} (JVMS§6.5)", CPLookup(&self.cp, CPEntryKind::Class, typ)));
				},
				0xbc => {
					let typ = self.read_u8()?;
					let type_str = match typ {
						4 => "boolean",
						5 => "char",
						6 => "float",
						7 => "double",
						8 => "byte",
						9 => "short",
						10 => "int",
						11 => "long",
						other => panic!("invalid newarray type {}", other),
					};
					self.explain_read(format!("newarray {} (JVMS§6.5)", type_str));
				}
				0x00 => self.explain_read("nop (JVMS§6.5)"),
				0x57 => self.explain_read("pop (JVMS§6.5)"),
				0x58 => self.explain_read("pop2 (JVMS§6.5)"),
				0xb5 => {
					let typ = self.read_cp()?;
					self.fi.explain_read(format!("putfield {} (JVMS§6.5)", CPLookup(&self.cp, CPEntryKind::Field, typ)));
				},
				0xb3 => {
					let typ = self.read_cp()?;
					self.fi.explain_read(format!("putstatic {} (JVMS§6.5)", CPLookup(&self.cp, CPEntryKind::Field, typ)));
				},
				0xa9 => self.explain_read("ret (JVMS§6.5)"),
				0xb1 => self.explain_read("return (JVMS§6.5)"),
				0x35 => self.explain_read("saload (JVMS§6.5)"),
				0x56 => self.explain_read("sastore (JVMS§6.5)"),
				0x11 => {
					let value = self.read_i16()? as i32;
					self.explain_read(format!("sipush {} (JVMS§6.5)", value))
				},
				0x5f => self.explain_read("swap (JVMS§6.5)"),
				0xaa => {
					let bc_offset = self.offset as i32;
					self.explain_read("tableswitch (JVMS§6.5)");
					self.indent(1);
					let padding = (self.offset - start) % 4;
					if padding > 0 {
						self.read_exact(&mut vec![0; 4 - padding])?;
						self.explain_read("padding")
					}
					let default = bc_offset + self.read_i32()?;
					self.explain_read(format!("default branch {:>}", default));
					let low = self.read_i32()?;
					self.explain_read("low bound");
					let high = self.read_i32()?;
					self.explain_read("high bound");
					self.indent(1);
					for key in low..high {
						let branch = bc_offset + self.read_i32()?;
						self.explain_read(format!("case {}: branch {:>}", key, branch));
					}
					self.indent(-2);
				}
				0xc4 => {
					self.explain_read("wide (JVMS§6.5)");
					if !is_wide {
						self.indent(1);
					}
					is_wide = true;
					continue;
				},
				_ => break,
			}
			if is_wide {
				self.indent(-1);
			}
			is_wide = false;
		}
		if self.offset + 1 < end {
			let mut buf = vec![0; end - (self.offset + 1)];
			self.read_exact(&mut buf)?;
			self.explain_read("remaining unknown bytecode");
		}
		if self.offset > end {
			panic!("over read end of bytecode block");
		}
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