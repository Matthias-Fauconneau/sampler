#![feature(bool_to_option)]
pub trait TryMap<T> { fn try_map<E, U, F:FnOnce(T)->Result<U, E>>(self, f: F) -> Result<Option<U>, E>; }
impl<T> TryMap<T> for Option<T> { fn try_map<E, U, F:FnOnce(T) ->Result<U, E>>(self, f: F) -> Result<Option<U>, E> { self.map(f).transpose() } }

pub trait TryThen { fn try_then<E, U, F:FnOnce()->Result<U, E>>(self, f: F) -> Result<Option<U>, E>; }
impl TryThen for bool { fn try_then<E, U, F:FnOnce() ->Result<U, E>>(self, f: F) -> Result<Option<U>, E> { self.then(f).transpose() } }

/*use nom::{IResult, error::{VerboseError, convert_error}};
#[throws] fn parse<'t,O>(parser: impl Fn(&'t str) -> IResult<&'t str, O, VerboseError<&'t str>>, s: &mut &'t str) -> O {
	impl From<nom::Err<String>> for Error {
		fn from(e: nom::Err<String>) -> Self {
			use nom::Err;
			match e {
				Err::Failure(e) | Err::Error(e) => Error::custom(e),
				Err::Incomplete(_) => Error::custom("Incomplete"),
			}
		}
	}
	let (r, v) = parser(*s).map_err(|e| e.map(|e| convert_error(*s, e)))?;
	*s = r;
	v
}*/

use framework::error::{throws,throw};
use serde::de::{self, Visitor, Error as de_Error, IntoDeserializer, Deserializer as de_Deserializer};

pub struct Deserializer<'de>{bytes: &'de [u8], terminator: Option<u8>}

#[derive(Debug)] pub struct Error(framework::Error);
impl std::fmt::Display for Error { fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result { self.0.fmt(f) } }
impl std::error::Error for Error {}
impl de::Error for Error { fn custom<T: std::fmt::Display>(msg: T) -> Self { Error(framework::Error::msg(msg.to_string())) } }
//pub fn parse_partial<N: FromLexical>(bytes: &[u8]) -> Result<(N, usize)>
macro_rules! bail { ($($arg:tt)*) => { throw!(Error::custom(format!($($arg)*))) } }
macro_rules! ensure { ($cond:expr, $($arg:tt)*) => { if !$cond { bail!($($arg)*) } } }

#[throws] pub fn parse_partial<R: lexical_core::FromLexical>(bytes: &mut &[u8]) -> R {
	let (v, n) = lexical_core::parse_partial(*bytes).map_err(|e| Error::custom(format!("{:?}",e)))?;
	*bytes = &bytes[n..];
	v
}

impl<'de> Deserializer<'de> {
	pub fn from_bytes(bytes: &'de [u8]) -> Self { Deserializer{bytes, terminator: None} }

	fn peekn(&self, n: usize) -> &'de [u8] { &self.bytes[0..n.min(self.bytes.len())] }
	fn peek(&self) -> Option<u8> { self.bytes.get(0).copied() }
    #[throws] fn next(&mut self) -> u8 { let b = self.peek().ok_or(Error::custom("Unexpected end of file"))?; self.bytes = &self.bytes[1..]; b }
	fn r#match(&mut self, b: u8) -> bool { if let Some(peek) = self.peek() { if peek == b { self.next().unwrap(); true } else { false } } else { false } }
	#[throws] fn skip(&mut self, b: u8) { ensure!(self.r#match(b), "Expected '{}', got '{:?}'", b, self.peek().unwrap_or(b'\x1C')); }

	fn while_not<P:Fn(&u8)->bool>(&mut self, predicate: P) -> &'de [u8] {
		let s = self.bytes.split(predicate).next().unwrap(); self.bytes = &self.bytes[s.len()..]; s
		//let (s, r) = self.bytes.split_at(self.bytes.iter().find(predicate).unwrap_or(self.0.len())); self.bytes = r; s
	}
	#[throws] fn until<P:Fn(&u8)->bool>(&mut self, pattern: P) -> &'de [u8] {
		let mut iter = self.bytes.splitn(2, pattern);
		let s = iter.next().unwrap();
		self.bytes = iter.next().ok_or(Error::custom(format!("Expected {:?}", "pattern")))?;
		s
	}
	fn r#while(&mut self, pattern: impl Fn(&u8)->bool) -> &'de [u8] { self.while_not(|c| !pattern(&c)) }
	fn skip_whitespace(&mut self) { self.r#while(u8::is_ascii_whitespace); }
}

impl<'de, 't> de::Deserializer<'de> for &'t mut Deserializer<'de> {
	type Error = Error;
	#[allow(unreachable_code)] #[throws] fn deserialize_any<V:Visitor<'de>>(self, visitor: V) -> V::Value {
		throw!(Error::invalid_type(serde::de::Unexpected::Other("unimplemented"), &visitor))
	}
	serde::forward_to_deserialize_any!{char bytes byte_buf option unit unit_struct tuple tuple_struct map ignored_any bool u16 u32 u64 u128 i16 i32 i64 i128 f64}

	#[throws] fn deserialize_seq<V:Visitor<'de>>(self, visitor: V) -> V::Value {
		struct Seq<'t, 'de: 't>(&'t mut Deserializer<'de>);
		impl<'de, 't> de::SeqAccess<'de> for Seq<'t, 'de> {
			type Error = Error;
			#[throws] fn next_element_seed<T:de::DeserializeSeed<'de>>(&mut self, seed: T) -> Option<T::Value> {
				self.0.skip_whitespace();
				/*if let Some(c) = self.0.peek() {
					if self.0.terminator.map_or(true, |t| &t != c) { Some(seed.deserialize(&mut *self.0)?) } else { None }
				} else { None }*/
				self.0.peek().try_map(|c| self.0.terminator.map_or(true, |t| t != c).try_then(|| seed.deserialize(&mut *self.0)) )?.flatten()
			}
		}
		visitor.visit_seq(Seq(&mut *self))?
	}

	#[throws] fn deserialize_struct<V:Visitor<'de>>(self, _name: &'static str, fields: &'static [&'static str], visitor: V) -> V::Value {
		struct Struct<'t, 'de: 't>{de: &'t mut Deserializer<'de>, fields: std::slice::Iter<'static, &'static str>};
		impl<'de, 't> de::MapAccess<'de> for Struct<'t, 'de> {
			type Error = Error;
			#[throws] fn next_key_seed<K:de::DeserializeSeed<'de>>(&mut self, seed: K) -> Option<K::Value> {
				self.fields.next().try_map::<Error,_,_>(|k| seed.deserialize(k.into_deserializer()))?
			}
			#[throws] fn next_value_seed<V:de::DeserializeSeed<'de>>(&mut self, seed: V) -> V::Value {
				let mut de = Deserializer{bytes: self.de.bytes, terminator: Some(b'<')};
				let value = seed.deserialize(&mut de)?;
				self.de.bytes = de.bytes;
				value
			}
		}
		visitor.visit_map(Struct{de: &mut *self, fields: fields.iter()})?
    }

	#[throws] fn deserialize_enum<V:Visitor<'de>>(self, _name: &'static str, _variants: &'static [&'static str], visitor: V) -> V::Value {
		if self.r#match(b'<') {
			visitor.visit_enum(std::str::from_utf8(self.until(|&c| c == b'>')?).map_err(|e| Error(e.into()))?.into_deserializer())?
			//visitor.visit_enum( IntoDeserializer::<'_,Error>::into_deserializer(self.until(|c| c == &b'>')?) )?
		} else {
			struct Enum<'t, 'de: 't>(&'t mut Deserializer<'de>);
			impl<'de, 't> de::EnumAccess<'de> for Enum<'t, 'de> {
				type Error = Error;
				type Variant = Self;
				#[throws] fn variant_seed<V:de::DeserializeSeed<'de>>(self, seed: V) -> (V::Value, Self::Variant) {
					let v = seed.deserialize(&mut *self.0)?;
					self.0.skip(b'=')?;
					(v, self)
				}
			}
			impl<'de, 'a> de::VariantAccess<'de> for Enum<'a, 'de> {
				type Error = Error;
				#[allow(unreachable_code)]#[throws] fn unit_variant(self) {}
				#[throws] fn newtype_variant_seed<T:de::DeserializeSeed<'de>>(self, seed: T) -> T::Value { seed.deserialize(self.0)? }
				#[throws] fn tuple_variant<V:Visitor<'de>>(self, _len: usize, visitor: V) -> V::Value { self.0.deserialize_seq(visitor)? }
				#[throws] fn struct_variant<V:Visitor<'de>>(self, _fields: &'static [&'static str], visitor: V) -> V::Value { self.0.deserialize_map(visitor)? }
			}
			visitor.visit_enum(Enum(self))?
		}
    }

    #[throws] fn deserialize_identifier<V:Visitor<'de>>(self, visitor: V) -> V::Value {
		let id = self.r#while(|&c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == b'_' || c == b'#');
		ensure!(!id.is_empty(), "Expected identifier, got '{}'", std::str::from_utf8(self.peekn(16)).map_err(|e| Error(e.into()))?);
    	visitor.visit_borrowed_str::<Error>(std::str::from_utf8(id).map_err(|e| Error(e.into()))?)?
	}
	#[throws] fn deserialize_str<V:Visitor<'de>>(self, visitor: V) -> V::Value { self.deserialize_identifier(visitor)? }
	#[throws] fn deserialize_string<V:Visitor<'de>>(self, visitor: V) -> V::Value {
		let path = self.while_not(|c| c.is_ascii_control());
		ensure!(!path.is_empty(), "Expected path string, got '{}'", std::str::from_utf8(self.peekn(16)).map_err(|e| Error(e.into()))?);
    	visitor.visit_string::<Error>(String::from_utf8(path.iter().map(|&b| if b==b'\\' { b'/' } else { b }).collect()).map_err(|e| Error(e.into()))?)?
	}

	#[throws] fn deserialize_u8<V:Visitor<'de>>(self, visitor: V) -> V::Value { visitor.visit_u8(parse_partial(&mut self.bytes)?)? }
	#[throws] fn deserialize_i8<V:Visitor<'de>>(self, visitor: V) -> V::Value { visitor.visit_i8(parse_partial(&mut self.bytes)?)? }
	#[throws] fn deserialize_f32<V:Visitor<'de>>(self, visitor: V) -> V::Value { visitor.visit_f32(parse_partial(&mut self.bytes)?)? }

	/*#[throws] fn deserialize_tuple_struct<V:Visitor<'de>>(self, _name: &'static str, len: usize, visitor: V) -> V::Value {
		struct Seq<'t, 'de: 't>{de: &'t mut Deserializer<'de>, remaining: usize};
		impl<'de, 't> de::SeqAccess<'de> for Seq<'t, 'de> {
			type Error = Error;
			#[throws] fn next_element_seed<T:de::DeserializeSeed<'de>>(&mut self, seed: T) -> Option<T::Value> {
				if self.remaining>0 { self.remaining -= 1; Some(seed.deserialize(&mut *self.de)?) } else { None }
			}
		}
		visitor.visit_seq(Seq{de: &mut *self, remaining: len})?
	}*/
	#[throws] fn deserialize_newtype_struct<V:Visitor<'de>>(self, _name: &'static str, visitor: V) -> V::Value { visitor.visit_newtype_struct(self)? }
}

use serde::Deserialize;
#[throws(framework::Error)] pub fn from_bytes<'t, T:Deserialize<'t>>(s: &'t [u8]) -> T {
	let mut deserializer = Deserializer::from_bytes(s);
	let t = T::deserialize(&mut deserializer)?;
	ensure!(deserializer.bytes.is_empty(), "Trailing characters: '{}'", std::str::from_utf8(deserializer.peekn(16)).map_err(|e| Error(e.into()))?);
	t
}

#[allow(non_camel_case_types)]#[derive(Deserialize, Debug)] enum Level { region, group, /*control, global, curve, effect, master, midi*/ }
#[allow(non_camel_case_types)] #[derive(Deserialize,Debug)] #[serde(try_from = "&str")] struct key(u8);
impl std::convert::TryFrom<&str> for key {
	type Error = Error;
	#[allow(unreachable_code)] #[throws] fn try_from(str: &str) -> Self {
		let s = str.as_bytes();
		let note = b"c#d#ef#g#a#b".iter().enumerate().filter(|(_,&c)| c!=b'#')
						.find({let f = s.first().ok_or(Error::custom("Invalid"))?; move |(_,&c)| c == *f}).ok_or(Error::custom("Invalid"))?.0;
		let mut s = &s[1..];
		let sharp = if let Some(b'#') = s.get(0) { s = &s[1..]; true } else { false };
		assert!(!s.is_empty(), "{}", str);
		key(12*(1+parse_partial::<i8>(&mut s)?) as u8 + note as u8 + if sharp { 1 } else { 0 })
	}
}
/*impl std::str::FromStr for key/*std::convert::TryFrom<&str>*/ {
	type Err = Self::Error;
	#[throws] fn from_str(s: &str) -> Self { s.try_from()? }
}*/
/*impl<'de> Deserialize<'de> for /*std::str::FromStr*/key {
    fn deserialize<D:serde::de::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> { str::deserialize(deserializer)?.parse() }
}*/

#[allow(non_camel_case_types)] #[derive(Deserialize,Debug)] struct velocity(u8);
#[allow(non_camel_case_types)] #[derive(Deserialize,Debug)] enum Opcode {
	// Definition
	sample(std::path::PathBuf), pitch_keycenter(key),
	// Condition
	lokey(key), hikey(key),
	lovel(velocity), hivel(velocity),
	lorand(f32), hirand(f32),
	locc64(u8), hicc64(u8),
	// Effect
	volume(i8), ampeg_release(f32), ampeg_attack(f32),
}
#[allow(non_camel_case_types)] #[derive(Deserialize,Debug)] pub struct Rule {
	level: Level,
	opcodes: Vec<Opcode>,
}

/*type Sample = vmap::Map;
/// When samples are to be played
#[derive(Default,Debug)] pub struct Condition {}
// How samples are to be played
#[derive(Default,Debug)] pub struct Effect {}

#[derive(Debug,Default)] pub struct Rule {
	pub sample: Option<Sample>,
	pub condition: Condition,
	pub effect: Effect,
}

impl<'de> Deserialize<'de> for Rule {
    fn deserialize<D:serde::de::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		/*struct OpcodeSeq;
        impl<'de> Visitor<'de> for OpcodeSeq {
            type Value = Rule;
            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result { f.write_str("opcode") }
            #[throws(A::Error)] fn visit_seq<A:serde::de::SeqAccess<'de>>(self, mut seq: A) -> Self::Value {
				let rule = Default::default();
                while let Some(item) = seq.next_element::<Opcode>()? {
                    match item {
						_ => panic!("{:?}", item),
                    }
                }
				rule //, {domain, function, map: map.ok_or(A::Error::missing_field("sample"))? })
            }
        }
        deserializer.deserialize_seq(OpcodeSeq)*/
        let rule = Default::default();
        while let Some(item) = Opcode::deserialize(deserializer)? {
			match item {
				_ => panic!("{:?}", item),
			}
		}
		rule
    }
}

#[allow(non_camel_case_types)] #[derive(Deserialize,Debug)] pub struct Component {
	level: Level,
	rule: Rule,
}*/
