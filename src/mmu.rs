// Memory management unit
//16-bit pointers - 64 KB of address space

//  0000       0100               4000                     8000       a000           c000     e000                fe00    fea0  ff00  ff80   ffff
// [ boot ROM |fixed ROM bank 00 | switchable ROM bank 01+|   VRAM   | External RAM | RAM    |        ???        |OAM RAM|  X  | I/O | HRAM |IER]
// |  in GB   |                on cartridge               |   in GB  | on cartridge |                        in GB                              |

/*
* 0000
* Boot ROM
* 00fe
* ==============
* 0100
* 16KB ROM - fixed ROM bank 00
* 3fff
* --
* 4000
* 16KB ROM - switchable ROM bank 01-NN
* 7fff
* ==============
* 8000
* 8KB VRAM (on GB)
* 9fff
* ==============
* a000
* 8KB External RAM (on cartridge, optional)
* bfff
* ==============
* c000
* 4KB Work RAM Bank 0 (WRAM)
* cfff
* --
* d000
* 4KB Work RAM Bank 1 (WRAM) (switchable bank 1-7 on CGB)
* dfff
* ==============
* e000
* ECHO (same as WRAM? but generally unused)
* fdff
* ==============
* fe00
* Object Attribute Memory (OAM) - sprite tables
* fe9f
* ==============
* fea0
* NOT USABLE
* feff
* ==============
* ff00
* I/O ports
* ff00 - P1 register (joypad info, system type info)
* ff01 - SB  - serial transfer data (R/W)
* ff02 - SC - SIO control
* 
* ff04 - DIV - divider register
* ff05 - TIMA - timer counter
* ff06 - TMA - timer modulo
* ff07 - TAC - timer control
*
* ff0f - IF - interrupt flag
*    bit 4 - Transition high to low on pins P10-P13 (1 - transition done, 0 - hasn't happened)
*    bit 3 - End of serial IO transfer
*    bit 2 - Timer overflow
*    bit 1 - LCD controller interrupt (LCDSTAT)
*    bit 0 - LCD VBlank pulse signal
*
* Pulse A voice registers
* ff10 - NR10 - Sound mode 1 register - Sweep
*    bit 6-4 - Sweep time
*    bit 3 - Sweep increase/decrease (0 - addition/frequency increase, 1 - subtraction/frequency decrease)
*    bit 2-0 - Sweep shift count
* ff11 - NR11 - Sound mode 1 register - Length/wave pattern duty
*    bit 7-6 - duty bits (default is 10)
*    bit 5-0 - length bits
* ff12 - NR12 - Sound mode 1 register - Envelope/volume sweep
*    bit 7-4 - Initial envelope volume
*    bit 3 - Envelope up or down? (0 - attenuate/down/like normal instruments, 1 - amplify/up/cool effect)
*    bit 2-0 - Sweep count
* ff13 - NR13 - Sound mode 1 register - Frequency (low bits)
* ff14 - NR14 - Sound mode 1 register - Frequency (high bits)
*    bit 7 - Trigger bit
*    bit 6 - Length bit (look at length register? y/n)
*    bit 2 - 0 - frequency's high 3 bits
*
* Pulse B voice registers
* ff16 - NR21 - Sound mode 2 register - Length/wave pattern duty
*    bit 7-6 - duty bits (default is 10)
*    bit 5-0 - length bits
* ff17 - NR22 - Sound mode 2 register - Envelope/volume sweep
*    bit 7-4 - Initial envelope volume
*    bit 3 - Envelope up or down? (0 - attenuate/down/like normal instruments, 1 - amplify/up/cool effect)
*    bit 2-0 - Sweep count
* ff18 - NR23 - Sound mode 2 register - Frequency (low bits)
* ff19 - NR24 - Sound mode 2 register - Frequency (high bits)
*    bit 7 - Trigger bit
*    bit 6 - Length bit (look at length register? y/n)
*    bit 2 - 0 - frequency's high 3 bits
*
* Wave voice registers
* ff1a - NR30 - Sound mode 3 register - Sound on/off
*    bit 7 - on/off
* ff1b - NR31 - Sound mode 3 register - Sound length
*    bit 7-0 - length bits
* ff1c - NR32 - Sound mode 3 register - Select output level
*    bits 6-5 - Select output level
* ff1d - NR33 - Sound mode 3 register - Frequency (low bits)
* ff1e - NR34 - Sound mode 3 register - Frequency (high bits)
*    bit 7 - Trigger bit
*    bit 6 - Length bit (look at length register? y/n)
*    bit 2 - 0 - frequency's high 3 bits
*
* Noise voice registers
* ff20 - NR41 - Sound mode 4 register - Length
*    bit 5-0 - length bits
* ff21 - NR42 - Sound mode 4 register - Envelope/volume sweep
*    bit 7-4 - Initial envelope volume
*    bit 3 - Envelope up or down? (0 - attenuate/down/like normal instruments, 1 - amplify/up/cool effect)
*    bit 2-0 - Sweep count
* ff22 - NR43 - Sound mode 4 register - Polynomial counter/pseudorandom noise generator
*    bit 7-4 - Selection of shift clock frequency of polynomial counter (1111 and 1110 invalid)
*    bit 3 - 0 for 15-bit, 1 for 7-bit mode
*    bit 2-0 - Dividing ratio of frequencies
* ff23 - NR44 - Sound mode 4 register - Counter/consecutive
*
* ff24 - NR50 - Channel control/on/off/volume
*    bit 7 - Enable output of analog input signal that nobody ever used 2
*    bit 6-4 - Left channel volume
*    bit 3 - Enable output of analog input signal that nobody ever used 1
*    bit 2-0 - Right channel volume
* ff25 - NR51 - Sound output terminal select
* ff26 - NR52 - Sound on/off per channel and overall
*    bit 7 - Enable/disable power to all sound hardware
*    bit 3-0 - Read-only status bits (can be written to, but won't do anything)
*
* ff30 - ff3f - Wave pattern RAM
*
* ff40 - LCDC
* ff41 - STAT
* ff42 - SCY - Scroll Y
* ff43 - SCX - Scroll X
* ff44 - LY - LCDC Y-coordinate
* ff45 - LYC - LY compare
* ff46 - DMA - DMA Transfer and start address
* ff47 - BGP - Background and window palette data
* ff48 - OBP0 - Object palette 0 data
* ff49 - OBP1 - Object palette 1 data
* ff4a - WY - Window Y position
* ff4b - WX - Window X position
*
* ff7f
* ==============
* ff80
* High RAM (HRAM)
* fffe
* ==============
* ffff - Interrupt Enable Register
*/
use std::io;
use std::io::prelude::*;
use std::fs::File;
pub struct AddressSpace
{
    pub mem : [u8; 65536]
}

pub fn ReadCartridge(memory: &mut [u8; 65536]) -> io::Result<()>
{
    //0100 - 3fff
    let mut cart = File::open("src/Tetris (World).gb")?;
    let mut ROMbank0 = [0; 16127];
    cart.read(&mut ROMbank0)?;

    memory[0x0100..0x3fff].clone_from_slice(&ROMbank0);

    Ok(())
}