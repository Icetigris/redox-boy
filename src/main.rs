#![allow(non_snake_case)]
const BIOS_ROM: [u8; 256] = [
0x31,0xfe, 0xff,0xaf, 0x21,0xff, 0x9f,0x32, 0xcb,0x7c, 0x20,0xfb, 0x21,0x26, 0xff,0x0e,
0x11,0x3e, 0x80,0x32, 0xe2,0x0c, 0x3e,0xf3, 0xe2,0x32, 0x3e,0x77, 0x77,0x3e, 0xfc,0xe0,
0x47,0x11, 0x04,0x01, 0x21,0x10, 0x80,0x1a, 0xcd,0x95, 0x00,0xcd, 0x96,0x00, 0x13,0x7b,
0xfe,0x34, 0x20,0xf3, 0x11,0xd8, 0x00,0x06, 0x08,0x1a, 0x13,0x22, 0x23,0x05, 0x20,0xf9,
0x3e,0x19, 0xea,0x10, 0x99,0x21, 0x2f,0x99, 0x0e,0x0c, 0x3d,0x28, 0x08,0x32, 0x0d,0x20,
0xf9,0x2e, 0x0f,0x18, 0xf3,0x67, 0x3e,0x64, 0x57,0xe0, 0x42,0x3e, 0x91,0xe0, 0x40,0x04,
0x1e,0x02, 0x0e,0x0c, 0xf0,0x44, 0xfe,0x90, 0x20,0xfa, 0x0d,0x20, 0xf7,0x1d, 0x20,0xf2,
0x0e,0x13, 0x24,0x7c, 0x1e,0x83, 0xfe,0x62, 0x28,0x06, 0x1e,0xc1, 0xfe,0x64, 0x20,0x06,
0x7b,0xe2, 0x0c,0x3e, 0x87,0xe2, 0xf0,0x42, 0x90,0xe0, 0x42,0x15, 0x20,0xd2, 0x05,0x20,
0x4f,0x16, 0x20,0x18, 0xcb,0x4f, 0x06,0x04, 0xc5,0xcb, 0x11,0x17, 0xc1,0xcb, 0x11,0x17,
0x05,0x20, 0xf5,0x22, 0x23,0x22, 0x23,0xc9, 0xce,0xed, 0x66,0x66, 0xcc,0x0d, 0x00,0x0b,
0x03,0x73, 0x00,0x83, 0x00,0x0c, 0x00,0x0d, 0x00,0x08, 0x11,0x1f, 0x88,0x89, 0x00,0x0e,
0xdc,0xcc, 0x6e,0xe6, 0xdd,0xdd, 0xd9,0x99, 0xbb,0xbb, 0x67,0x63, 0x6e,0x0e, 0xec,0xcc,
0xdd,0xdc, 0x99,0x9f, 0xbb,0xb9, 0x33,0x3e, 0x3c,0x42, 0xb9,0xa5, 0xb9,0xa5, 0x42,0x3c,
0x21,0x04, 0x01,0x11, 0xa8,0x00, 0x1a,0x13, 0xbe,0x20, 0xfe,0x23, 0x7d,0xfe, 0x34,0x20,
0xf5,0x06, 0x19,0x78, 0x86,0x23, 0x05,0x20, 0xfb,0x86, 0x20,0xfe, 0x3e,0x01, 0xe0,0x50];

fn Pack16(hi: u8, lo: u8) -> u16
{
    return ((hi as u16) << 8) | (lo as u16);
}

fn PCReadByte(memory: &[u8; 65536], cycles: &mut u32, PC: &mut u16) -> u8
{
    let byte = memory[*PC as usize];
    (*PC) += 1;
    (*cycles) += 4;
    return byte;
}

fn ReadByte(memory: &[u8; 65536], cycles: &mut u32, readSrcAddr: u16) -> u8
{
    let byte = memory[readSrcAddr as usize];
    (*cycles) += 4;
    return byte;
}

fn WriteByte(memory: &mut [u8; 65536], cycles: &mut u32, byte: u8, writeDest: u16)
{
    memory[writeDest as usize] = byte;
    (*cycles) += 4;
}

fn WriteHL(cycles: &mut u32, hl: u16, H: &mut u8, L: &mut u8)
{
    *H = (hl >> 8) as u8;
    *L = (0x00ff & hl) as u8;
    (*cycles) += 4;
}

fn PushStack(memory: &mut [u8; 65536], cycles: &mut u32, PC: u16, SP: &mut u16)
{
    //save PC at current stack address
    let hiPC: u8 = (PC >> 8) as u8;
    let loPC: u8 = (PC & 0x00ff) as u8;
    println!("Saving PC (${:04x}) at SP (${:04x}).", PC, SP);
    memory[*SP as usize] = loPC;
    (*SP) -= 1;
    memory[*SP as usize] = hiPC;
    (*SP) -= 1;
    //move stack pointer down (stack grows downwards in address space)
    println!("SP moved to ${:04x}", SP);
    (*cycles) += 4;
}

fn main()
{    
    //CPU
    //registers
    let mut A: u8 = 0; //accumulator
    let mut F: u8 = 0; //flags: [Z, N, H, C, -, -, -, -]
                        //Z = zero, N = negative, H = half-carry, C = carry
    let mut B: u8 = 0;
    let mut C: u8 = 0;

    let mut D: u8 = 0;
    let mut E: u8 = 0;

    let mut H: u8 = 0;
    let mut L: u8 = 0;
    // HL can hold a memory address, which you can put into SP with LD SP,HL

    let mut SP: u16 = 0; //stack pointer
    let mut PC: u16 = 0;

    let mut cpuCycles: u32 = 0;

    //MMU
    //address space
    let mut memory: [u8; 65536] = [0; 65536];
    memory[..256].clone_from_slice(&BIOS_ROM);

    loop
    {
        //PC == 0 at start
        println!{"current byte at {:04x}: {:02x}", PC, memory[PC as usize]};

        //instruction decode
        let currentByte = PCReadByte(&memory, &mut cpuCycles, &mut PC);
        match currentByte
        {
            0x00 => println!("NOP"),
            0x10 => println!("STOP"),
            // immediate loads
            0x0e =>
            {
                C = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                println!("LD C, {:02x}", C);
            }
            0x3e =>
            {
                A = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                println!("LD A, {:02x}", A);
            }
            // relative jumps
            0x18 =>
            { 
                //JR
            }
            0x20 =>
            {
                //JR NZ - last result not zero?: PC +/- signed immediate
                let offset: u8 = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                println!("JR NZ offset {}", offset);
                if F & 0x80 != 0 // check Z flag
                {
                    if offset > 0x7f //if immediate is larger than 127
                    {
                        //this number is negative, so 2s complement into an unsigned absolute value we can subtract
                        let signedOffset: u8 = (!offset + 1) & 0xff;
                        PC -= signedOffset as u16;
                        println!("JR NZ PC - offset {}, {}", PC, signedOffset);
                    }
                    else
                    {
                        PC += offset as u16;
                        println!("JR NZ PC + offset {}", PC);
                    }
                }
            },
            0x28 =>
            { 
                //JR Z
            }
            0x30 =>
            {
                //JR NC
            }
            0x38 =>
            { 
                //JR C
            }
            0xcd =>
            {
                let jumpDestLo = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                let jumpDestHi = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                PushStack(&mut memory, &mut cpuCycles, PC, &mut SP);
                PC = Pack16(jumpDestHi, jumpDestLo);
                println!("CALL ${:02x}{:02x}", jumpDestHi, jumpDestLo);
            },
            // 8-bit register increments
            0x04 => 
            {
                B += 1;
                println!("INC B");
            },
            0x14 => 
            {
                D += 1;
                println!("INC D");
            },
            0x24 => 
            {
                H += 1;
                println!("INC H");
            },
            0x0c => 
            {
                C += 1;
                println!("INC C");
            },
            0x1c => 
            {
                E += 1;
                println!("INC E");
            },
            0x2c => 
            {
                L += 1;
                println!("INC L");
            },
            0x3c => 
            {
                A += 1;
                println!("INC A");
            },
            // 16-bit loads
            0x01 => 
            {
                C = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                B = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                println!("LD BC, ${:02x}{:02x}", B, C);
            },
            0x11 => 
            {
                E = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                D = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                println!("LD DE, ${:02x}{:02x}", D, E);
            },
            0x21 => 
            {
                L = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                H = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                println!("LD HL, ${:02x}{:02x}", H, L);
            },
            0x31 => 
            {
                SP = PCReadByte(&memory, &mut cpuCycles, &mut PC) as u16;
                SP |= (PCReadByte(&memory, &mut cpuCycles, &mut PC) as u16) << 8;
                println!("LD SP, ${:04x}", SP);
            },
            0x32 => 
            {
                //memory[HL] = A
                //HL--
                let hl: u16 = ((H as u16) << 8) | (L as u16);
                println!("mem at {:04x}: {:04x}", hl, memory[hl as usize]);
                WriteByte(&mut memory, &mut cpuCycles, A, hl);
                println!("HL, ${:02x}{:02x}", H, L);
                WriteHL(&mut cpuCycles, hl - 1, &mut H, &mut L);
                println!("HL, ${:02x}{:02x}", H, L);
                println!("LD (HL-), A", );
            },
            // XORs
            0xaf =>
            {
                A ^= A;
                if A == 0
                {
                    F = 0x8; // 1000
                }
                println!("XOR A: ${:04x}", A);
                println!("F (ZNHC): {:02b}", F);
            },
            0xa8 =>
            {
                A ^= B;
                if A == 0
                {
                    F = 0x8; // 1000
                }
                println!("XOR A, B: ${:04x}, ${:04x}", A, B);
                println!("F (ZNHC): {:04b}", F);
            },
            0xa9 => println!("XOR C"),
            0xaa => println!("XOR D"),
            0xab => println!("XOR E"),
            0xac => println!("XOR H"),
            0xad => println!("XOR L"),
            0xae => println!("XOR HL"),
            0xee => println!("XOR d8"),
            0x1a =>
            {
                //load value from address in DE into A
                //A = memory[DE]
                let de = Pack16(D, E);
                A = ReadByte(&memory, &mut cpuCycles, de);
                println!("LD A, (DE): load A with data at address in DE. A ({:02x}) = mem[${:02x}{:02x}]", A, D, E);
            },
            0x77 =>
            {
                // store A at address in HL
                let destAddr = Pack16(H, L);
                WriteByte(&mut memory, &mut cpuCycles, A, destAddr);
                println!("LD (HL), A: store {:02x} at (${:02x}{:02x})", A, H, L);
                println!("mem[${:04x}]: {:02x}", destAddr, memory[destAddr as usize]);
            },
            0xe0 =>
            {
                // store A at memory address $FF00 + immediate value
                let immediate: u8 = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                let destAddr: u16 = 0xff00 + immediate as u16;
                WriteByte(&mut memory, &mut cpuCycles, A, destAddr);
                println!("LDH (n),A: store {:02x} at $FF00 + {:02x} (${:04x})", A, immediate, destAddr);
                println!("mem[${:04x}]: {:02x}", destAddr, memory[destAddr as usize]);
            },
            0xe2 =>
            {
                // store A at address $FF00 + C
                let destAddr: u16 = 0xff00 + C as u16;
                WriteByte(&mut memory, &mut cpuCycles, A, destAddr);
                println!("LD (C),A: store {:02x} at $FF00 + {:02x} (${:04x})", A, C, destAddr);
                println!("mem[${:04x}]: {:02x}", destAddr, memory[destAddr as usize]);
            },
            // Prefix CB
            0xcb =>
            {
                println!("PREFIX CB");
                let cb = PCReadByte(&memory, &mut cpuCycles, &mut PC);

                match cb
                {
                    0x00 => println!("RLC B"),
                    0x01 => println!("RLC C"),
                    0x02 => println!("RLC D"),
                    0x03 => println!("RLC E"),
                    0x04 => println!("RLC H"),
                    0x05 => println!("RLC L"),
                    0x06 => println!("RLC (HL)"),
                    0x07 => println!("RLC A"),
                    0x08 => println!("RRC B"),
                    0x09 => println!("RRC C"),
                    0x0a => println!("RRC D"),
                    0x0b => println!("RRC E"),
                    0x0c => println!("RRC H"),
                    0x0d => println!("RRC L"),
                    0x0e => println!("RRC (HL)"),
                    0x0f => println!("RRC A"),
                    0x10 => println!("RL B"),
                    0x11 => println!("RL C"),
                    0x12 => println!("RL D"),
                    0x13 => println!("RL E"),
                    0x14 => println!("RL H"),
                    0x15 => println!("RL L"),
                    0x16 => println!("RL (HL)"),
                    0x17 => println!("RL A"),
                    0x18 => println!("RR B"),
                    0x19 => println!("RR C"),
                    0x1a => println!("RR D"),
                    0x1b => println!("RR E"),
                    0x1c => println!("RR H"),
                    0x1d => println!("RR L"),
                    0x1e => println!("RR (HL)"),
                    0x1f => println!("RR A"),
                    0x20 => println!("SLA B"),
                    0x21 => println!("SLA C"),
                    0x22 => println!("SLA D"),
                    0x23 => println!("SLA E"),
                    0x24 => println!("SLA H"),
                    0x25 => println!("SLA L"),
                    0x26 => println!("SLA (HL)"),
                    0x27 => println!("SLA A"),
                    0x28 => println!("SRA B"),
                    0x29 => println!("SRA C"),
                    0x2a => println!("SRA D"),
                    0x2b => println!("SRA E"),
                    0x2c => println!("SRA H"),
                    0x2d => println!("SRA L"),
                    0x2e => println!("SRA (HL)"),
                    0x2f => println!("SRA A"),
                    0x30 => println!("SWAP B"),
                    0x31 => println!("SWAP C"),
                    0x32 => println!("SWAP D"),
                    0x33 => println!("SWAP E"),
                    0x34 => println!("SWAP H"),
                    0x35 => println!("SWAP L"),
                    0x36 => println!("SWAP (HL)"),
                    0x37 => println!("SWAP A"),
                    0x38 => println!("SRL B"),
                    0x39 => println!("SRL C"),
                    0x3a => println!("SRL D"),
                    0x3b => println!("SRL E"),
                    0x3c => println!("SRL H"),
                    0x3d => println!("SRL L"),
                    0x3e => println!("SRL (HL)"),
                    0x3f => println!("SRL A"),
                    0x40 => println!("BIT 0, B"),
                    0x41 => println!("BIT 0, C"),
                    0x42 => println!("BIT 0, D"),
                    0x43 => println!("BIT 0, E"),
                    0x44 => println!("BIT 0, H"),
                    0x45 => println!("BIT 0, L"),
                    0x46 => println!("BIT 0, (HL)"),
                    0x47 => println!("BIT 0, A"),
                    0x48 => println!("BIT 1, B"),
                    0x49 => println!("BIT 1, C"),
                    0x4a => println!("BIT 1, D"),
                    0x4b => println!("BIT 1, E"),
                    0x4c => println!("BIT 1, H"),
                    0x4d => println!("BIT 1, L"),
                    0x4e => println!("BIT 1, (HL)"),
                    0x4f => println!("BIT 1, A"),
                    0x50 => println!("BIT 2, B"),
                    0x51 => println!("BIT 2, C"),
                    0x52 => println!("BIT 2, D"),
                    0x53 => println!("BIT 2, E"),
                    0x54 => println!("BIT 2, H"),
                    0x55 => println!("BIT 2, L"),
                    0x56 => println!("BIT 2, (HL)"),
                    0x57 => println!("BIT 2, A"),
                    0x58 => println!("BIT 3, B"),
                    0x59 => println!("BIT 3, C"),
                    0x5a => println!("BIT 3, D"),
                    0x5b => println!("BIT 3, E"),
                    0x5c => println!("BIT 3, H"),
                    0x5d => println!("BIT 3, L"),
                    0x5e => println!("BIT 3, (HL)"),
                    0x5f => println!("BIT 3, A"),
                    0x60 => println!("BIT 4, B"),
                    0x61 => println!("BIT 4, C"),
                    0x62 => println!("BIT 4, D"),
                    0x63 => println!("BIT 4, E"),
                    0x64 => println!("BIT 4, H"),
                    0x65 => println!("BIT 4, L"),
                    0x66 => println!("BIT 4, (HL)"),
                    0x67 => println!("BIT 4, A"),
                    0x68 => println!("BIT 5, B"),
                    0x69 => println!("BIT 5, C"),
                    0x6a => println!("BIT 5, D"),
                    0x6b => println!("BIT 5, E"),
                    0x6c => println!("BIT 5, H"),
                    0x6d => println!("BIT 5, L"),
                    0x6e => println!("BIT 5, (HL)"),
                    0x6f => println!("BIT 5, A"),
                    0x70 => println!("BIT 6, B"),
                    0x71 => println!("BIT 6, C"),
                    0x72 => println!("BIT 6, D"),
                    0x73 => println!("BIT 6, E"),
                    0x74 => println!("BIT 6, H"),
                    0x75 => println!("BIT 6, L"),
                    0x76 => println!("BIT 6, (HL)"),
                    0x77 => println!("BIT 6, A"),
                    0x78 => println!("BIT 7, B"),
                    0x79 => println!("BIT 7, C"),
                    0x7a => println!("BIT 7, D"),
                    0x7b => println!("BIT 7, E"),
                    0x7c => 
                    {
                        // 1000 0000
                        if H & 0x80 == 0
                        {
                            let fc = F & 0x01;
                            F = 0xa | fc; //1010
                        }
                        else
                        {
                            let fc = F & 0x01;
                            F = 0x2 | fc; //0010
                        }
                        println!("BIT 7, H: {:02x} ({:08b})", H,H);
                        println!("F (ZNHC): {:04b}", F);
                    },
                    0x7d => println!("BIT 7, L"),
                    0x7e => println!("BIT 7, (HL)"),
                    0x7f => println!("BIT 7, A"),
                    0x80 => println!("RES 0, B"),
                    0x81 => println!("RES 0, C"),
                    0x82 => println!("RES 0, D"),
                    0x83 => println!("RES 0, E"),
                    0x84 => println!("RES 0, H"),
                    0x85 => println!("RES 0, L"),
                    0x86 => println!("RES 0, (HL)"),
                    0x87 => println!("RES 0, A"),
                    0x88 => println!("RES 1, B"),
                    0x89 => println!("RES 1, C"),
                    0x8a => println!("RES 1, D"),
                    0x8b => println!("RES 1, E"),
                    0x8c => println!("RES 1, H"),
                    0x8d => println!("RES 1, L"),
                    0x8e => println!("RES 1, (HL)"),
                    0x8f => println!("RES 1, A"),
                    0x90 => println!("RES 2, B"),
                    0x91 => println!("RES 2, C"),
                    0x92 => println!("RES 2, D"),
                    0x93 => println!("RES 2, E"),
                    0x94 => println!("RES 2, H"),
                    0x95 => println!("RES 2, L"),
                    0x96 => println!("RES 2, (HL)"),
                    0x97 => println!("RES 2, A"),
                    0x98 => println!("RES 3, B"),
                    0x99 => println!("RES 3, C"),
                    0x9a => println!("RES 3, D"),
                    0x9b => println!("RES 3, E"),
                    0x9c => println!("RES 3, H"),
                    0x9d => println!("RES 3, L"),
                    0x9e => println!("RES 3, (HL)"),
                    0x9f => println!("RES 3, A"),
                    0xa0 => println!("RES 4, B"),
                    0xa1 => println!("RES 4, C"),
                    0xa2 => println!("RES 4, D"),
                    0xa3 => println!("RES 4, E"),
                    0xa4 => println!("RES 4, H"),
                    0xa5 => println!("RES 4, L"),
                    0xa6 => println!("RES 4, (HL)"),
                    0xa7 => println!("RES 4, A"),
                    0xa8 => println!("RES 5, B"),
                    0xa9 => println!("RES 5, C"),
                    0xaa => println!("RES 5, D"),
                    0xab => println!("RES 5, E"),
                    0xac => println!("RES 5, H"),
                    0xad => println!("RES 5, L"),
                    0xae => println!("RES 5, (HL)"),
                    0xaf => println!("RES 5, A"),
                    0xb0 => println!("RES 6, B"),
                    0xb1 => println!("RES 6, C"),
                    0xb2 => println!("RES 6, D"),
                    0xb3 => println!("RES 6, E"),
                    0xb4 => println!("RES 6, H"),
                    0xb5 => println!("RES 6, L"),
                    0xb6 => println!("RES 6, (HL)"),
                    0xb7 => println!("RES 6, A"),
                    0xb8 => println!("RES 7, B"),
                    0xb9 => println!("RES 7, C"),
                    0xba => println!("RES 7, D"),
                    0xbb => println!("RES 7, E"),
                    0xbc => println!("RES 7, H"),
                    0xbd => println!("RES 7, L"),
                    0xbe => println!("RES 7, (HL)"),
                    0xbf => println!("RES 7, A"),
                    0xc0 => println!("SET 0, B"),
                    0xc1 => println!("SET 0, C"),
                    0xc2 => println!("SET 0, D"),
                    0xc3 => println!("SET 0, E"),
                    0xc4 => println!("SET 0, H"),
                    0xc5 => println!("SET 0, L"),
                    0xc6 => println!("SET 0, (HL)"),
                    0xc7 => println!("SET 0, A"),
                    0xc8 => println!("SET 1, B"),
                    0xc9 => println!("SET 1, C"),
                    0xca => println!("SET 1, D"),
                    0xcb => println!("SET 1, E"),
                    0xcc => println!("SET 1, H"),
                    0xcd => println!("SET 1, L"),
                    0xce => println!("SET 1, (HL)"),
                    0xcf => println!("SET 1, A"),
                    0xd0 => println!("SET 2, B"),
                    0xd1 => println!("SET 2, C"),
                    0xd2 => println!("SET 2, D"),
                    0xd3 => println!("SET 2, E"),
                    0xd4 => println!("SET 2, H"),
                    0xd5 => println!("SET 2, L"),
                    0xd6 => println!("SET 2, (HL)"),
                    0xd7 => println!("SET 2, A"),
                    0xd8 => println!("SET 3, B"),
                    0xd9 => println!("SET 3, C"),
                    0xda => println!("SET 3, D"),
                    0xdb => println!("SET 3, E"),
                    0xdc => println!("SET 3, H"),
                    0xdd => println!("SET 3, L"),
                    0xde => println!("SET 3, (HL)"),
                    0xdf => println!("SET 3, A"),
                    0xe0 => println!("SET 4, B"),
                    0xe1 => println!("SET 4, C"),
                    0xe2 => println!("SET 4, D"),
                    0xe3 => println!("SET 4, E"),
                    0xe4 => println!("SET 4, H"),
                    0xe5 => println!("SET 4, L"),
                    0xe6 => println!("SET 4, (HL)"),
                    0xe7 => println!("SET 4, A"),
                    0xe8 => println!("SET 5, B"),
                    0xe9 => println!("SET 5, C"),
                    0xea => println!("SET 5, D"),
                    0xeb => println!("SET 5, E"),
                    0xec => println!("SET 5, H"),
                    0xed => println!("SET 5, L"),
                    0xee => println!("SET 5, (HL)"),
                    0xef => println!("SET 5, A"),
                    0xf0 => println!("SET 6, B"),
                    0xf1 => println!("SET 6, C"),
                    0xf2 => println!("SET 6, D"),
                    0xf3 => println!("SET 6, E"),
                    0xf4 => println!("SET 6, H"),
                    0xf5 => println!("SET 6, L"),
                    0xf6 => println!("SET 6, (HL)"),
                    0xf7 => println!("SET 6, A"),
                    0xf8 => println!("SET 7, B"),
                    0xf9 => println!("SET 7, C"),
                    0xfa => println!("SET 7, D"),
                    0xfb => println!("SET 7, E"),
                    0xfc => println!("SET 7, H"),
                    0xfd => println!("SET 7, L"),
                    0xfe => println!("SET 7, (HL)"),
                    0xff => println!("SET 7, A"),
                }
            },

            _ => println!("not an opcode"),
        }


        if PC > 255
        {
            break;
        }
    }
}
