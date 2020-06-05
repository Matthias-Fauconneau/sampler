#[allow(unreachable_code)] #[fehler::throws(anyhow::Error)] fn main() {
    //let file = std::fs::read("piano/IvyAudio-PianoIn162-Close.sfz")?;
    //let sfz : Vec<sfz::Sample> = sfz::from_str(&std::str::from_utf8(&file)?)?;
    let sfz : Vec<sfz::Sample> = sfz::from_str(&std::str::from_utf8(&std::fs::read("../piano/IvyAudio-PianoIn162-Close.sfz")?)?)?;
    anyhow::bail!("{:?}", sfz);
	/*let midi = alsa::rawmidi::Rawmidi::new("hw:1,0", alsa::Direction::Capture, false)?;
	let info = midi.info()?;
	eprintln!("{} {} {} {:?} {} {}", midi.name()?, info.get_device(), info.get_subdevice(), info.get_stream(), info.get_subdevice_name()?, info.get_id()?);
	loop {
		let mut buffer = [0u8; 1];
		use std::io::Read;
		match midi.io().read(&mut buffer) {
			Ok(1) => println!("{} {:x}", buffer[0], buffer[0]),
			e => panic!("{:?}", e),
		}
	}*/
}
