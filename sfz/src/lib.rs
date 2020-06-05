#![allow(non_camel_case_types)]

use serde::Deserialize;

#[derive(Deserialize,Debug)] struct key(u8);
#[derive(Deserialize,Debug)] struct velocity(u8);
#[derive(Deserialize,Debug)] enum Item {
	// Header
	region, group, //control, global, curve, effect, master, midi,
	// Opcode
	sample(std::path::PathBuf), ampeg_release(f32), ampeg_attack(f32), lokey(key), hikey(key), hivel(velocity), hirand(f32), pitch_keycenter(key), volume(u8), hicc64(u8)
}

pub struct Deserializer<'t> { input: &'t str }
impl<'t> Deserializer<'t> { pub fn from_str(input: &'t str) -> Self { Deserializer { input } } }
use fehler::throws;
#[throws(framework::Error)] pub fn from_str<'t, T:Deserialize<'t>>(s: &'t str) -> T {
	let mut deserializer = Deserializer::from_str(s);
	let t = T::deserialize(&mut deserializer)?;
	framework::ensure!(deserializer.input.is_empty());
	t
}

#[derive(Debug)] pub struct Error(framework::Error);
impl std::fmt::Display for Error { fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result { self.0.fmt(f) } }
impl std::error::Error for Error {}
impl serde::de::Error for Error { fn custom<T: std::fmt::Display>(msg: T) -> Self { Error(framework::Error::msg(msg.to_string())) } }

use serde::de::Visitor;
impl<'de, 't> serde::de::Deserializer<'de> for &'t mut Deserializer<'de> {
	#![allow(unreachable_code, unused_variables)]
	type Error = self::Error;
	#[throws] fn deserialize_any<V:Visitor<'de>>(self, visitor: V) -> V::Value { unimplemented!() }
	#[throws] fn deserialize_bool<V:Visitor<'de>>(self, visitor: V) -> V::Value { unimplemented!() }
	#[throws] fn deserialize_i8<V:Visitor<'de>>(self, visitor: V) -> V::Value { unimplemented!() }
	#[throws] fn deserialize_i16<V:Visitor<'de>>(self, visitor: V) -> V::Value { unimplemented!() }
	#[throws] fn deserialize_i32<V:Visitor<'de>>(self, visitor: V) -> V::Value { unimplemented!() }
	#[throws] fn deserialize_i64<V:Visitor<'de>>(self, visitor: V) -> V::Value { unimplemented!() }
	#[throws] fn deserialize_u8<V:Visitor<'de>>(self, visitor: V) -> V::Value { unimplemented!() }
	#[throws] fn deserialize_u16<V:Visitor<'de>>(self, visitor: V) -> V::Value { unimplemented!() }
	#[throws] fn deserialize_u32<V:Visitor<'de>>(self, visitor: V) -> V::Value { unimplemented!() }
	#[throws] fn deserialize_u64<V:Visitor<'de>>(self, visitor: V) -> V::Value { unimplemented!() }
	#[throws] fn deserialize_f32<V:Visitor<'de>>(self, visitor: V) -> V::Value { unimplemented!() }
	#[throws] fn deserialize_f64<V:Visitor<'de>>(self, visitor: V) -> V::Value { unimplemented!() }
	#[throws] fn deserialize_char<V:Visitor<'de>>(self, visitor: V) -> V::Value { unimplemented!() }
	#[throws] fn deserialize_str<V:Visitor<'de>>(self, visitor: V) -> V::Value { unimplemented!() }
	#[throws] fn deserialize_string<V:Visitor<'de>>(self, visitor: V) -> V::Value { unimplemented!() }
	#[throws] fn deserialize_bytes<V:Visitor<'de>>(self, visitor: V) -> V::Value { unimplemented!() }
	#[throws] fn deserialize_byte_buf<V:Visitor<'de>>(self, visitor: V) -> V::Value { unimplemented!() }
	#[throws] fn deserialize_option<V:Visitor<'de>>(self, visitor: V) -> V::Value { unimplemented!() }
	#[throws] fn deserialize_unit<V:Visitor<'de>>(self, visitor: V) -> V::Value { unimplemented!() }
	#[throws] fn deserialize_unit_struct<V:Visitor<'de>>(self, name: &'static str, visitor: V) -> V::Value { unimplemented!() }
	#[throws] fn deserialize_newtype_struct<V:Visitor<'de>>(self, name: &'static str, visitor: V) -> V::Value { unimplemented!() }
	#[throws] fn deserialize_seq<V:Visitor<'de>>(self, visitor: V) -> V::Value { unimplemented!() }
	#[throws] fn deserialize_tuple<V:Visitor<'de>>(self, len: usize, visitor: V) -> V::Value { unimplemented!() }
	#[throws] fn deserialize_tuple_struct<V:Visitor<'de>>(self, name: &'static str, len: usize, visitor: V) -> V::Value { unimplemented!() }
	#[throws] fn deserialize_map<V:Visitor<'de>>(self, visitor: V) -> V::Value { unimplemented!() }
	#[throws] fn deserialize_struct<V:Visitor<'de>>(self,  name: &'static str, fields: &'static [&'static str], visitor: V) -> V::Value { unimplemented!() }
	#[throws] fn deserialize_enum<V:Visitor<'de>>(self, name: &'static str, variants: &'static [&'static str],visitor: V) -> V::Value { unimplemented!() }
	#[throws] fn deserialize_identifier<V:Visitor<'de>>(self, visitor: V) -> V::Value { unimplemented!() }
	#[throws] fn deserialize_ignored_any<V:Visitor<'de>>(self, visitor: V) -> V::Value { unimplemented!() }
}


#[derive(Default,Debug)] pub struct Domain {}
#[derive(Default,Debug)] pub struct Function {}
#[derive(Debug)] pub struct Sample {
	pub domain: Domain,
	pub function: Function,
	pub map: vmap::Map,
}
impl<'de> Deserialize<'de> for Sample {
    fn deserialize<D:serde::de::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
		struct ItemVisitor();
        impl<'de> Visitor<'de> for ItemVisitor {
            type Value = Sample;
            fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result { f.write_str("items") }
            fn visit_seq<A:serde::de::SeqAccess<'de>>(self, mut seq: A) -> Result<Self::Value, A::Error> {
				#![allow(unused_mut)]
				let mut domain = Default::default();
				let mut function = Default::default();
				let mut map = None;
                while let Some(item) = seq.next_element::<Item>()? {
                    match item {
						_ => panic!("{:?}", item),
                    }
                }
                use serde::de::Error;
				Ok(Sample{domain, function, map: map.ok_or(A::Error::missing_field("sample"))? })
            }
        }
        deserializer.deserialize_seq(ItemVisitor())
    }
}
