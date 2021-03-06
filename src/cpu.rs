//machine cycles: 1.048576 MHz
//clock speed: 4.194304 MHz
//4.295454 SGB, 4.194/8.388MHz GBC

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
    let mut cartOffset = 0;

    if readSrcAddr > 0x100
    {
        cartOffset = 0x100;
    }

    let byte = memory[readSrcAddr as usize + cartOffset];
    (*cycles) += 4;
    return byte;
}

fn WriteByte(memory: &mut [u8; 65536], cycles: &mut u32, byte: u8, writeDest: u16)
{
    if writeDest >= 0xff00
    {
        println!("write: {:04X} {:02X}", writeDest, byte);
    }
    memory[writeDest as usize] = byte;
    (*cycles) += 4;
}

fn Unpack8(reg16: u16, regHi: &mut u8, regLo: &mut u8)
{
    *regHi = (reg16 >> 8) as u8;
    *regLo = (0x00ff & reg16) as u8;
}

fn WriteHL(hl: u16, H: &mut u8, L: &mut u8)
{
    *H = (hl >> 8) as u8;
    *L = (0x00ff & hl) as u8;
}

fn PushStack(memory: &mut [u8; 65536], cycles: &mut u32, regHi: u8, regLo: u8, SP: &mut u16)
{
    //save address in registers at current stack address
    //println!("save ${:02x}{:02x} at ${:04x}", regHi, regLo, SP); 
    memory[*SP as usize] = regLo;
    (*SP) -= 1;
    (*cycles) += 4;
    memory[*SP as usize] = regHi;
    (*SP) -= 1;
    (*cycles) += 4;
    //move stack pointer down (stack grows downwards in address space)
    //println!("SP moved to ${:04x}", SP);
    (*cycles) += 4;
}

fn Call(memory: &mut [u8; 65536], cycles: &mut u32, PC: u16, SP: &mut u16)
{
    //save PC at current stack address
    //println!("save ${:04x} at ${:04x}", PC, SP);
    let hiAddressBits: u8 = (PC >> 4) as u8;
    let loAddressBits: u8 = (PC & 0x0f) as u8;
    memory[*SP as usize] = loAddressBits;
    (*SP) -= 1;
    (*cycles) += 4;
    memory[*SP as usize] = hiAddressBits;
    (*SP) -= 1;
    (*cycles) += 4;
    //move stack pointer down (stack grows downwards in address space)
    //println!("SP moved to ${:04x}", SP);
    (*cycles) += 4;
}

fn PopStack(memory: &[u8; 65536], cycles: &mut u32, regHi: &mut u8, regLo: &mut u8, SP: &mut u16)
{
    (*SP) += 1;
    (*cycles) += 4;
    *regHi = memory[*SP as usize];
    (*SP) += 1;
    (*cycles) += 4;
    *regLo = memory[*SP as usize];
    //println!("SP moved to ${:04x}", SP);
}

fn Return(memory: &[u8; 65536], cycles: &mut u32, PC: &mut u16, SP: &mut u16)
{
    // Pop 16 bits off stack and jump to that address
    (*SP) += 1;
    (*cycles) += 4;
    let hiPC: u8 = memory[*SP as usize];
    (*SP) += 1;
    (*cycles) += 4;
    let loPC: u8 = memory[*SP as usize];
    *PC = ((hiPC as u16) << 4) | loPC as u16;
    (*cycles) += 4;
    //println!("load ${:04x} from ${:04x}", PC, *SP - 2);
    //println!("SP moved to ${:04x}", SP);
}

fn SetZ(F: &mut u8)
{
    *F |= 0x80; //1000 0000
}

fn ResetZ(F: &mut u8)
{
    *F &= 0x70; // 0111 0000
}

fn SetN(F : &mut u8)
{
    *F |= 0x40; // 0100 0000
}

fn ResetN(F : &mut u8)
{
    *F &= 0xb0; // 1011 0000
}

fn SetH(F : &mut u8)
{
    *F |= 0x20; // 0010 0000
}

fn ResetH(F : &mut u8)
{
    *F &= 0xd0; // 1101 0000
}

fn SetC(F : &mut u8)
{
    *F |= 0x10; // 0001 0000
}

fn ResetZN(F : &mut u8)
{
    *F &= 0x30; // 0011 0000
}

fn Increment(register: &mut u8, F: &mut u8)
{
    *register = (*register) + 1;
    
    *F = 0;
    // would use __readeflags intrinsic here IF IT WAS SUPPORTED
    if *register == 0
    {
        SetZ(&mut *F);
    }

    ResetN(&mut *F);

    //set H if carry from bit 3 (??)
    if *register & 0xf == 0
    {
        SetH(&mut *F);
    }
}

fn Decrement(register: &mut u8, F: &mut u8)
{
    *register = (*register) - 1;
    
    *F = *F & 0x10; // preserve C flag; mask with 0001 0000
    // would use __readeflags intrinsic here IF IT WAS SUPPORTED
    if *register == 0
    {
        SetZ(&mut *F);
    }

    SetN(&mut *F);

    // set H if no borrow from bit 4 (??)
    if *register & 0xf == 0xf
    {
        ResetH(&mut *F);
    }
}

fn Compare(A: u8, register: u8, F: &mut u8)
{
    *F = 0;
    SetN(&mut *F);
    if A == register
    {
        SetZ(&mut *F);
    }
    else if A < register
    {
        SetC(&mut *F);
    }

    // set H if no borrow from bit 4 (??)
    if register & 0xf == 0xf
    {
        SetH(&mut *F);
    }
}

fn RotateLeftThroughCarry(register: &mut u8, F: &mut u8)
{
    // 1101 0000->
    // 1010 0001 (C holds 1 that came off the front and puts it back on the end)
    // Z = 1 if result is 0
    // N = 0
    // H = 0
    // C = whatever was in bit 7
    let oldCF = (*F >> 4) & 0x1;

    // mask off bit 7 of what's in the register, right shift by 3 to stick it in C flag slot of F
    // 1000 0000 >> 3 = 0001 0000
    *F = (*register & 0x80) >> 3;

    //left shift C
    //put what's in C back into the register in bit slot 0
    //right shift F by 4 slots to put it in bit slot 0
    *register = (*register << 1) | oldCF;

    //using the intrinsic saves me a couple of instructions with -C opt-level=3
    //*register = (*register as u16).rotate_left(1) as u8;

    //how do I set the Z flag without this shitty jump
    if *register == 0
    {
        SetZ(&mut *F);
    }
}

fn RotateLeft(register: &mut u8, F: &mut u8)
{
    // 1101 0000->
    // 1010 0001 (C holds 1 that came off the front and puts it back on the end)
    // Z = 1 if result is 0
    // N = 0
    // H = 0
    // C = whatever was in bit 7

    // mask off bit 7 of what's in the register, right shift by 3 to stick it in C flag slot of F
    // 1000 0000 >> 3 = 0001 0000
    *F = (*register & 0x80) >> 3;

    //left shift C
    //put what's in C back into the register in bit slot 0
    //right shift F by 4 slots to put it in bit slot 0
    //*register = (*register << 1) | (*F >> 4);

    //using the intrinsic saves me a couple of instructions with -C opt-level=3
    *register = (*register).rotate_left(1);

    //how do I set the Z flag without this shitty jump
    if *register == 0
    {
        SetZ(&mut *F);
    }
}

fn Xor(A: &mut u8, register: u8, F: &mut u8)
{
    *A ^= register;
    if *A == 0
    {
        *F = 0x80; // 1000
    }
}

pub fn Run(mem: &mut [u8; 65536])
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

    // clock cycles
    let mut cpuCycles: u32 = 0;

    //MMU
    let mut memory = mem;

    let mut loops: u32 = 0;
    loop
    {
        //PC == 0 at start
        //println!("============================================================================");
        if loops > 24590
        {
            //println!("{} cycles: {} PC: {:04X} SP: {:04X} A: {:02X} B: {:02X} C: {:02X} D: {:02X} E: {:02X} H: {:02X} L: {:02X} F: {:02X}", loops, cpuCycles, PC, SP, A, B, C, D, E, H, L, F);
            //println!("current byte at PC ({:04x}): {:02x}", PC, memory[PC as usize]);
        }
        //println!("F (ZNHC): {:08b}", F);
        //println!("SP: {:04x}", SP);
        //println!("current byte at PC ({:04x}): {:02x}", PC, memory[PC as usize]);
        loops = loops + 1;
        if loops == 28816 || loops == 29000
        {
            println!("turgle");
        }
        //instruction decode
        let currentByte = PCReadByte(&memory, &mut cpuCycles, &mut PC);
        match currentByte
        {
            0x00 => println!("NOP"),
            0x10 => println!("STOP"),
            // relative jumps
            0x18 =>
            {
                //JR jump: PC +/- signed immediate
                let offset: u8 = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                cpuCycles += 4;
                if offset > 0x7f //if immediate is larger than 127
                {
                    //this number is negative, so 2s complement into an unsigned absolute value we can subtract
                    let signedOffset: u8 = (!offset + 1) & 0xff;
                    PC -= signedOffset as u16;
                    //println!("JR PC - offset {}, {}", PC, signedOffset);
                    //println!("JR ${:04x}", PC);
                }
                else
                {
                    PC += offset as u16;
                    //println!("JR PC + offset {}", PC);
                }
            }
            0x20 =>
            {
                //JR NZ - last result not zero?: PC +/- signed immediate
                let offset: u8 = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                //println!("JR NZ offset {}", offset);
                if F & 0x80 == 0 // jump if Z flag is 0
                {
                    cpuCycles += 4;
                    if offset > 0x7f //if immediate is larger than 127
                    {
                        //this number is negative, so 2s complement into an unsigned absolute value we can subtract
                        let signedOffset: u8 = (!offset + 1) & 0xff;
                        PC -= signedOffset as u16;
                        //println!("JR NZ PC - offset {}, {}", PC, signedOffset);
                        //println!("JR NZ ${:04x}", PC);
                    }
                    else
                    {
                        PC += offset as u16;
                        //println!("JR NZ PC + offset {}", PC);
                    }
                }
            },
            0x28 =>
            {
                //JR Z - last result zero?: PC +/- signed immediate
                let offset: u8 = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                //println!("JR Z offset {}", offset);
                if F & 0x80 != 0 // jump if Z flag is 1
                {
                    cpuCycles += 4;
                    if offset > 0x7f //if immediate is larger than 127
                    {
                        //this number is negative, so 2s complement into an unsigned absolute value we can subtract
                        let signedOffset: u8 = (!offset + 1) & 0xff;
                        PC -= signedOffset as u16;
                        //println!("JR Z PC - offset {}, {}", PC, signedOffset);
                        //println!("JR Z ${:04x}", PC);
                    }
                    else
                    {
                        PC += offset as u16;
                        //println!("JR Z PC + offset {}", PC);
                    }
                }
            }
            //0x30 =>
            //{
            //    //JR NC
            //}
            //0x38 =>
            //{ 
            //    //JR C
            //}
            0x2f =>
            {
                //complement A
                A = !A;
                SetN(&mut F);
                SetH(&mut F);
                println!("CPL A");
            }
            0xc5 =>
            {
                // PUSH BC
                //println!("PUSH BC (${:02x}{:02x}) at SP (${:04x}).", B, C, SP);
                PushStack(&mut memory, &mut cpuCycles, B, C, &mut SP);
            },
            0xd5 =>
            {
                // PUSH DE
                //println!("PUSH DE (${:02x}{:02x}) at SP (${:04x}).", D, E, SP);
                PushStack(&mut memory, &mut cpuCycles, D, E, &mut SP);
            },
            0xe5 =>
            {
                // PUSH HL
                //println!("PUSH HL (${:02x}{:02x}) at SP (${:04x}).", H, L, SP);
                PushStack(&mut memory, &mut cpuCycles, H, L, &mut SP);
            },
            0xf5 =>
            {
                // PUSH AF
                //println!("PUSH AF (${:02x}{:02x}) at SP (${:04x}).", A, F, SP);
                PushStack(&mut memory, &mut cpuCycles, A, F, &mut SP);
            },
            0xcd =>
            {
                let jumpDestLo = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                let jumpDestHi = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                //println!("Saving PC (${:04x}) at SP (${:04x}).", PC, SP);
                Call(&mut memory, &mut cpuCycles, PC, &mut SP);
                PC = Pack16(jumpDestHi, jumpDestLo);
                //println!("CALL ${:02x}{:02x}", jumpDestHi, jumpDestLo);
            },
            0xc1 =>
            {
                PopStack(&memory, &mut cpuCycles, &mut B, &mut C, &mut SP);
                //println!("POP BC (${:02x}{:02x}) at SP (${:04x}).", B, C, SP);
            },
            0xc9 =>
            {
                Return(&memory, &mut cpuCycles, &mut PC, &mut SP);
                //println!("RET (${:04x}) at SP (${:04x}).", PC, SP);
            },
            0xd1 =>
            {
                PopStack(&memory, &mut cpuCycles, &mut D, &mut E, &mut SP);
                //println!("POP DE (${:02x}{:02x}) at SP (${:04x}).", D, E, SP);
            },
            0xe1 =>
            {
                PopStack(&memory, &mut cpuCycles, &mut H, &mut L, &mut SP);
                //println!("POP HL (${:02x}{:02x}) at SP (${:04x}).", H, L, SP);
            },
            0xf1 =>
            {
                PopStack(&memory, &mut cpuCycles, &mut A, &mut F, &mut SP);
                //println!("POP AF (${:02x}{:02x}) at SP (${:04x}).", A, F, SP);
            },
            // 8-bit register increments
            0x04 => 
            {
                Increment(&mut B, &mut F);
                //println!("INC B");
            },
            0x14 => 
            {
                Increment(&mut D, &mut F);
                //println!("INC D");
            },
            0x24 => 
            {
                Increment(&mut H, &mut F);
                //println!("INC H");
            },
            0x0c => 
            {
                Increment(&mut C, &mut F);
                //println!("INC C");
            },
            0x1c => 
            {
                Increment(&mut E, &mut F);
                //println!("INC E");
            },
            0x2c => 
            {
                Increment(&mut L, &mut F);
                //println!("INC L");
            },
            0x3c => 
            {
                Increment(&mut A, &mut F);
                //println!("INC A");
            },
            // 8-bit register decrements
            0x05 => 
            {
                Decrement(&mut B, &mut F);
                //println!("DEC B");
            },
            0x15 => 
            {
                Decrement(&mut D, &mut F);
                //println!("DEC D");
            },
            0x25 => 
            {
                Decrement(&mut H, &mut F);
                //println!("DEC H");
            },
            0x0d => 
            {
                Decrement(&mut C, &mut F);
                //println!("DEC C");
            },
            0x1d => 
            {
                Decrement(&mut E, &mut F);
                //println!("DEC E");
            },
            0x2d => 
            {
                Decrement(&mut L, &mut F);
                //println!("DEC L");
            },
            0x3d => 
            {
                Decrement(&mut A, &mut F);
                //println!("DEC A");
            },
            // 16-bit register increments
            //0x03 => println!("INC BC"),
            0x13 => 
            {
                let de: u16 = ((D as u16) << 8) | (E as u16);
                Unpack8(de + 1, &mut D, &mut E);
                cpuCycles += 4;
                //println!("INC DE");
            },
            0x23 =>
            {
                let hl: u16 = ((H as u16) << 8) | (L as u16);
                WriteHL(hl + 1, &mut H, &mut L);
                cpuCycles += 4;
                //println!("INC HL, ${:02x}{:02x}", H, L);
            },
            //0x33 => println!("INC SP"),
            0x17 =>
            {
                RotateLeftThroughCarry(&mut A, &mut F);
                ResetZ(&mut F);
                //println!("RL A");
            },
            // increment value at address in HL
            //0x34 => println!("INC (HL)"),
            // 8-bit immediate loads
            0x06 => 
            {
                B = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                //println!("LD B,n: {:02x}", B);
            },
            0x0e => 
            {
                C = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                //println!("LD C,n: {:02x}", C);
            },
            0x16 => 
            {
                D = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                //println!("LD D,n: {:02x}", D);
            },
            0x1e => 
            {
                E = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                //println!("LD E,n: {:02x}", E);
            },
            0x26 => 
            {
                H = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                //println!("LD H,n: {:02x}", H);
            },
            0x2e => 
            {
                L = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                //println!("LD L,n: {:02x}", L);
            },
            0x3e =>
            {
                A = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                //println!("LD A, {:02x}", A);
            }
            // 8-bit register loads
            0x40 => 
            {
                B = B;
                //println!("LD B,B: {:02x}", B);
            },
            0x41 => 
            {
                B = C;
                //println!("LD B,C: {:02x}", C);
            },
            0x42 => 
            {
                B = D;
                //println!("LD B,D: {:02x}", D);
            },
            0x43 => 
            {
                B = E;
                //println!("LD B,E: {:02x}", E);
            },
            0x44 => 
            {
                B = H;
                //println!("LD B,H: {:02x}", H);
            },
            0x45 => 
            {
                B = L;
                //println!("LD B,L: {:02x}", L);
            },
            0x47 => 
            {
                B = A;
                //println!("LD B,A: {:02x}", A);
            },
            0x48 => 
            {
                C = B;
                //println!("LD C,B: {:02x}", B);
            },
            0x49 => 
            {
                C = C;
                //println!("LD C,C: {:02x}", C);
            },
            0x4a => 
            {
                C = D;
                //println!("LD C,D: {:02x}", D);
            },
            0x4b => 
            {
                C = E;
                //println!("LD C,E: {:02x}", E);
            },
            0x4c => 
            {
                C = H;
                //println!("LD C,H: {:02x}", H);
            },
            0x4d => 
            {
                C = L;
                //println!("LD C,L: {:02x}", L);
            },
            0x4f =>
            {
                C = A;
                //println!("LD C,A: {:02x}", C);
            },
            0x50 => 
            {
                D = B;
                //println!("LD D,B: {:02x}", B);
            },
            0x51 => 
            {
                D = C;
                //println!("LD D,C: {:02x}", C);
            },
            0x52 => 
            {
                D = D;
                //println!("LD D,D: {:02x}", D);
            },
            0x53 => 
            {
                D = E;
                //println!("LD D,E: {:02x}", E);
            },
            0x54 => 
            {
                D = H;
                //println!("LD D,H: {:02x}", H);
            },
            0x55 => 
            {
                D = L;
                //println!("LD D,L: {:02x}", L);
            },
            0x57 => 
            {
                D = A;
                println!("LD D,A: {:02x}", A);
            },
            0x58 => 
            {
                E = B;
                //println!("LD E,B: {:02x}", B);
            },
            0x59 => 
            {
                E = C;
                //println!("LD E,C: {:02x}", C);
            },
            0x5a => 
            {
                E = D;
                //println!("LD E,D: {:02x}", D);
            },
            0x5b => 
            {
                E = E;
                //println!("LD E,E: {:02x}", E);
            },
            0x5c => 
            {
                E = H;
                //println!("LD E,H: {:02x}", H);
            },
            0x5d => 
            {
                E = L;
                //println!("LD E,L: {:02x}", L);
            },
            0x5f => 
            {
                E = A;
                //println!("LD E,A: {:02x}", A);
            },
            0x60 => 
            {
                H = B;
                //println!("LD H,B: {:02x}", B);
            },
            0x61 => 
            {
                H = C;
                //println!("LD H,C: {:02x}", C);
            },
            0x62 => 
            {
                H = D;
                //println!("LD H,D: {:02x}", D);
            },
            0x63 => 
            {
                H = E;
                //println!("LD H,E: {:02x}", E);
            },
            0x64 => 
            {
                H = H;
                //println!("LD H,H: {:02x}", H);
            },
            0x65 => 
            {
                H = L;
                //println!("LD H,L: {:02x}", L);
            },
            0x67 => 
            {
                H = A;
                //println!("LD H,A: {:02x}", A);
            },
            0x68 => 
            {
                L = B;
                //println!("LD L,B: {:02x}", B);
            },
            0x69 => 
            {
                L = C;
                //println!("LD L,C: {:02x}", C);
            },
            0x6a => 
            {
                L = D;
                //println!("LD L,D: {:02x}", D);
            },
            0x6b => 
            {
                L = E;
                //println!("LD L,E: {:02x}", E);
            },
            0x6c => 
            {
                L = H;
                //println!("LD L,H: {:02x}", H);
            },
            0x6d => 
            {
                L = L;
                //println!("LD L,L: {:02x}", L);
            },
            0x6f => 
            {
                L = A;
                //println!("LD L,A: {:02x}", A);
            },
            0x78 => 
            {
                A = B;
                //println!("LD A,B: {:02x}", B);
            },
            0x79 => 
            {
                A = C;
                //println!("LD A,C: {:02x}", C);
            },
            0x7a => 
            {
                A = D;
                //println!("LD A,D: {:02x}", D);
            },
            0x7b => 
            {
                A = E;
                //println!("LD A,E: {:02x}", E);
            },
            0x7c => 
            {
                A = H;
                //println!("LD A,H: {:02x}", H);
            },
            0x7d => 
            {
                A = L;
                //println!("LD A,L: {:02x}", L);
            },
            0x7f => 
            {
                A = A;
                //println!("LD A,A: {:02x}", A);
            },
            // 16-bit loads
            0x01 => 
            {
                C = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                B = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                //println!("LD BC, ${:02x}{:02x}", B, C);
            },
            0x11 => 
            {
                E = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                D = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                //println!("LD DE, ${:02x}{:02x}", D, E);
            },
            0x21 => 
            {
                L = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                H = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                //println!("LD HL, ${:02x}{:02x}", H, L);
            },
            0x22 => 
            {
                //memory[HL] = A
                //HL++
                let hl: u16 = ((H as u16) << 8) | (L as u16);
                //println!("mem at {:04x}: {:04x}", hl, memory[hl as usize]);
                WriteByte(&mut memory, &mut cpuCycles, A, hl);
                //println!("HL, ${:02x}{:02x}", H, L);
                WriteHL(hl + 1, &mut H, &mut L);
                //println!("HL, ${:02x}{:02x}", H, L);
                //println!("LD (HL+), A", );
            },
            0x31 => 
            {
                SP = PCReadByte(&memory, &mut cpuCycles, &mut PC) as u16;
                SP |= (PCReadByte(&memory, &mut cpuCycles, &mut PC) as u16) << 8;
                //println!("LD SP, ${:04x}", SP);
            },
            0x32 => 
            {
                //memory[HL] = A
                //HL--
                let hl: u16 = ((H as u16) << 8) | (L as u16);
                //println!("mem at {:04x}: {:04x}", hl, memory[hl as usize]);
                WriteByte(&mut memory, &mut cpuCycles, A, hl);
                //println!("HL, ${:02x}{:02x}", H, L);
                WriteHL(hl - 1, &mut H, &mut L);
                //println!("HL, ${:02x}{:02x}", H, L);
                //println!("LD (HL-), A", );
            },
            // XORs
            0xaf =>
            {
                let a = A;
                Xor(&mut A, a, &mut F);
                //println!("XOR A: ${:04x}", A);
                //println!("F (ZNHC): {:08b}", F);
            },
            0xa8 =>
            {
                Xor(&mut A, B, &mut F);
                //println!("XOR A, B: ${:04x}, ${:04x}", A, B);
                //println!("F (ZNHC): {:08b}", F);
            },
            //0xa9 => println!("XOR C"),
            //0xaa => println!("XOR D"),
            //0xab => println!("XOR E"),
            //0xac => println!("XOR H"),
            //0xad => println!("XOR L"),
            //0xae => println!("XOR HL"),
            //0xee => println!("XOR d8"),
            0x1a =>
            {
                //load value from address in DE into A
                //A = memory[DE]
                let de = Pack16(D, E);
                A = ReadByte(&memory, &mut cpuCycles, de);
                //println!("LD A, (DE): load A with data at address in DE. A ({:02x}) = mem[${:02x}{:02x}]", A, D, E);
            },
            0x77 =>
            {
                // store A at address in HL
                let destAddr = Pack16(H, L);
                WriteByte(&mut memory, &mut cpuCycles, A, destAddr);
                //println!("LD (HL), A: store {:02x} at (${:02x}{:02x})", A, H, L);
                //println!("mem[${:04x}]: {:02x}", destAddr, memory[destAddr as usize]);
            },
            0xe0 =>
            {
                // store A at memory address $FF00 + immediate value
                let immediate: u8 = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                let destAddr: u16 = 0xff00 + immediate as u16;
                WriteByte(&mut memory, &mut cpuCycles, A, destAddr);
                //println!("LDH (n),A: store {:02x} at $FF00 + {:02x} (${:04x})", A, immediate, destAddr);
                //println!("mem[${:04x}]: {:02x}", destAddr, memory[destAddr as usize]);
            },
            0xe2 =>
            {
                // store A at address $FF00 + C
                let destAddr: u16 = 0xff00 + C as u16;
                WriteByte(&mut memory, &mut cpuCycles, A, destAddr);
                //println!("LD (C),A: store {:02x} at $FF00 + {:02x} (${:04x})", A, C, destAddr);
                //println!("mem[${:04x}]: {:02x}", destAddr, memory[destAddr as usize]);
            },
            0xea =>
            {
                // store A at address
                let destLo: u8 = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                let destHi: u8 = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                let destAddr = Pack16(destHi, destLo);
                WriteByte(&mut memory, &mut cpuCycles, A, destAddr);
                //println!("LD (a16),A: store {:02x} at (${:04x})", A,  destAddr);
                //println!("mem[${:04x}]: {:02x}", destAddr, memory[destAddr as usize]);
            },
            0xf0 =>
            {
                //LDH A,n
                // A = memory[$FF00 + offset]
                let offset = PCReadByte(&memory, &mut cpuCycles, &mut PC) as u16;
                A = memory[(0xff00 + offset) as usize]; //should be 0x7a, but is 0 for some fucking reason (vertical scroll register isn't updating?)
                cpuCycles += 4;
                println!("LDH A,$ff{:02x}", offset);
            }
            0xfe =>
            {
                //CP immediate; A - imm, but toss results
                let imm = PCReadByte(&memory, &mut cpuCycles, &mut PC);
                Compare(A, imm, &mut F);
                //println!("CP A(${:02x}),${:02x}", A, imm);
            },
            // Prefix CB
            0xcb =>
            {
                //println!("PREFIX CB");
                let cb = PCReadByte(&memory, &mut cpuCycles, &mut PC);

                match cb
                {
                    //0x00 => println!("RLC B"),
                    //0x01 => println!("RLC C"),
                    //0x02 => println!("RLC D"),
                    //0x03 => println!("RLC E"),
                    //0x04 => println!("RLC H"),
                    //0x05 => println!("RLC L"),
                    //0x06 => println!("RLC (HL)"),
                    //0x07 => println!("RLC A"),
                    //0x08 => println!("RRC B"),
                    //0x09 => println!("RRC C"),
                    //0x0a => println!("RRC D"),
                    //0x0b => println!("RRC E"),
                    //0x0c => println!("RRC H"),
                    //0x0d => println!("RRC L"),
                    //0x0e => println!("RRC (HL)"),
                    //0x0f => println!("RRC A"),
                    0x10 => 
                    {
                        RotateLeftThroughCarry(&mut B, &mut F);
                        //println!("RL B");
                    },
                    0x11 => 
                    {
                        RotateLeftThroughCarry(&mut C, &mut F);
                        //println!("RL C");
                    },
                    0x12 => 
                    {
                        RotateLeftThroughCarry(&mut D, &mut F);
                        //println!("RL D");
                    },
                    0x13 => 
                    {
                        RotateLeftThroughCarry(&mut E, &mut F);
                        //println!("RL E");
                    },
                    0x14 => 
                    {
                        RotateLeftThroughCarry(&mut H, &mut F);
                        //println!("RL H");
                    },
                    0x15 => 
                    {
                        RotateLeftThroughCarry(&mut L, &mut F);
                        //println!("RL L");
                    },
                    //0x16 => println!("RL (HL)"),
                    0x17 =>
                    {
                        RotateLeftThroughCarry(&mut A, &mut F);
                        //println!("RL A");
                    },
                    //0x18 => println!("RR B"),
                    //0x19 => println!("RR C"),
                    //0x1a => println!("RR D"),
                    //0x1b => println!("RR E"),
                    //0x1c => println!("RR H"),
                    //0x1d => println!("RR L"),
                    //0x1e => println!("RR (HL)"),
                    //0x1f => println!("RR A"),
                    //0x20 => println!("SLA B"),
                    //0x21 => println!("SLA C"),
                    //0x22 => println!("SLA D"),
                    //0x23 => println!("SLA E"),
                    //0x24 => println!("SLA H"),
                    //0x25 => println!("SLA L"),
                    //0x26 => println!("SLA (HL)"),
                    //0x27 => println!("SLA A"),
                    //0x28 => println!("SRA B"),
                    //0x29 => println!("SRA C"),
                    //0x2a => println!("SRA D"),
                    //0x2b => println!("SRA E"),
                    //0x2c => println!("SRA H"),
                    //0x2d => println!("SRA L"),
                    //0x2e => println!("SRA (HL)"),
                    //0x2f => println!("SRA A"),
                    //0x30 => println!("SWAP B"),
                    //0x31 => println!("SWAP C"),
                    //0x32 => println!("SWAP D"),
                    //0x33 => println!("SWAP E"),
                    //0x34 => println!("SWAP H"),
                    //0x35 => println!("SWAP L"),
                    //0x36 => println!("SWAP (HL)"),
                    //0x37 => println!("SWAP A"),
                    //0x38 => println!("SRL B"),
                    //0x39 => println!("SRL C"),
                    //0x3a => println!("SRL D"),
                    //0x3b => println!("SRL E"),
                    //0x3c => println!("SRL H"),
                    //0x3d => println!("SRL L"),
                    //0x3e => println!("SRL (HL)"),
                    //0x3f => println!("SRL A"),
                    //0x40 => println!("BIT 0, B"),
                    //0x41 => println!("BIT 0, C"),
                    //0x42 => println!("BIT 0, D"),
                    //0x43 => println!("BIT 0, E"),
                    //0x44 => println!("BIT 0, H"),
                    //0x45 => println!("BIT 0, L"),
                    //0x46 => println!("BIT 0, (HL)"),
                    //0x47 => println!("BIT 0, A"),
                    //0x48 => println!("BIT 1, B"),
                    //0x49 => println!("BIT 1, C"),
                    //0x4a => println!("BIT 1, D"),
                    //0x4b => println!("BIT 1, E"),
                    //0x4c => println!("BIT 1, H"),
                    //0x4d => println!("BIT 1, L"),
                    //0x4e => println!("BIT 1, (HL)"),
                    //0x4f => println!("BIT 1, A"),
                    //0x50 => println!("BIT 2, B"),
                    //0x51 => println!("BIT 2, C"),
                    //0x52 => println!("BIT 2, D"),
                    //0x53 => println!("BIT 2, E"),
                    //0x54 => println!("BIT 2, H"),
                    //0x55 => println!("BIT 2, L"),
                    //0x56 => println!("BIT 2, (HL)"),
                    //0x57 => println!("BIT 2, A"),
                    //0x58 => println!("BIT 3, B"),
                    //0x59 => println!("BIT 3, C"),
                    //0x5a => println!("BIT 3, D"),
                    //0x5b => println!("BIT 3, E"),
                    //0x5c => println!("BIT 3, H"),
                    //0x5d => println!("BIT 3, L"),
                    //0x5e => println!("BIT 3, (HL)"),
                    //0x5f => println!("BIT 3, A"),
                    //0x60 => println!("BIT 4, B"),
                    //0x61 => println!("BIT 4, C"),
                    //0x62 => println!("BIT 4, D"),
                    //0x63 => println!("BIT 4, E"),
                    //0x64 => println!("BIT 4, H"),
                    //0x65 => println!("BIT 4, L"),
                    //0x66 => println!("BIT 4, (HL)"),
                    //0x67 => println!("BIT 4, A"),
                    //0x68 => println!("BIT 5, B"),
                    //0x69 => println!("BIT 5, C"),
                    //0x6a => println!("BIT 5, D"),
                    //0x6b => println!("BIT 5, E"),
                    //0x6c => println!("BIT 5, H"),
                    //0x6d => println!("BIT 5, L"),
                    //0x6e => println!("BIT 5, (HL)"),
                    //0x6f => println!("BIT 5, A"),
                    //0x70 => println!("BIT 6, B"),
                    //0x71 => println!("BIT 6, C"),
                    //0x72 => println!("BIT 6, D"),
                    //0x73 => println!("BIT 6, E"),
                    //0x74 => println!("BIT 6, H"),
                    //0x75 => println!("BIT 6, L"),
                    //0x76 => println!("BIT 6, (HL)"),
                    //0x77 => println!("BIT 6, A"),
                    //0x78 => println!("BIT 7, B"),
                    //0x79 => println!("BIT 7, C"),
                    //0x7a => println!("BIT 7, D"),
                    //0x7b => println!("BIT 7, E"),
                    0x7c => 
                    {
                        // test bit 7 in H
                        // Z = 1 if bit 7 is 0
                        // N = 0
                        // H = 1
                        // 1000 0000
                        if H & 0x80 == 0
                        {
                            let fc = F & 0xa0;
                            F = 0xa0 | fc; //1010 0000
                        }
                        else
                        {
                            let fc = F & 0x20;
                            F = 0x20 | fc; //0010 0000
                        }
                        //println!("BIT 7, H: {:02x} ({:08b})", H,H);
                        //println!("F (ZNHC): {:08b}", F);
                    },
                    //0x7d => println!("BIT 7, L"),
                    //0x7e => println!("BIT 7, (HL)"),
                    //0x7f => println!("BIT 7, A"),
                    //0x80 => println!("RES 0, B"),
                    //0x81 => println!("RES 0, C"),
                    //0x82 => println!("RES 0, D"),
                    //0x83 => println!("RES 0, E"),
                    //0x84 => println!("RES 0, H"),
                    //0x85 => println!("RES 0, L"),
                    //0x86 => println!("RES 0, (HL)"),
                    //0x87 => println!("RES 0, A"),
                    //0x88 => println!("RES 1, B"),
                    //0x89 => println!("RES 1, C"),
                    //0x8a => println!("RES 1, D"),
                    //0x8b => println!("RES 1, E"),
                    //0x8c => println!("RES 1, H"),
                    //0x8d => println!("RES 1, L"),
                    //0x8e => println!("RES 1, (HL)"),
                    //0x8f => println!("RES 1, A"),
                    //0x90 => println!("RES 2, B"),
                    //0x91 => println!("RES 2, C"),
                    //0x92 => println!("RES 2, D"),
                    //0x93 => println!("RES 2, E"),
                    //0x94 => println!("RES 2, H"),
                    //0x95 => println!("RES 2, L"),
                    //0x96 => println!("RES 2, (HL)"),
                    //0x97 => println!("RES 2, A"),
                    //0x98 => println!("RES 3, B"),
                    //0x99 => println!("RES 3, C"),
                    //0x9a => println!("RES 3, D"),
                    //0x9b => println!("RES 3, E"),
                    //0x9c => println!("RES 3, H"),
                    //0x9d => println!("RES 3, L"),
                    //0x9e => println!("RES 3, (HL)"),
                    //0x9f => println!("RES 3, A"),
                    //0xa0 => println!("RES 4, B"),
                    //0xa1 => println!("RES 4, C"),
                    //0xa2 => println!("RES 4, D"),
                    //0xa3 => println!("RES 4, E"),
                    //0xa4 => println!("RES 4, H"),
                    //0xa5 => println!("RES 4, L"),
                    //0xa6 => println!("RES 4, (HL)"),
                    //0xa7 => println!("RES 4, A"),
                    //0xa8 => println!("RES 5, B"),
                    //0xa9 => println!("RES 5, C"),
                    //0xaa => println!("RES 5, D"),
                    //0xab => println!("RES 5, E"),
                    //0xac => println!("RES 5, H"),
                    //0xad => println!("RES 5, L"),
                    //0xae => println!("RES 5, (HL)"),
                    //0xaf => println!("RES 5, A"),
                    //0xb0 => println!("RES 6, B"),
                    //0xb1 => println!("RES 6, C"),
                    //0xb2 => println!("RES 6, D"),
                    //0xb3 => println!("RES 6, E"),
                    //0xb4 => println!("RES 6, H"),
                    //0xb5 => println!("RES 6, L"),
                    //0xb6 => println!("RES 6, (HL)"),
                    //0xb7 => println!("RES 6, A"),
                    //0xb8 => println!("RES 7, B"),
                    //0xb9 => println!("RES 7, C"),
                    //0xba => println!("RES 7, D"),
                    //0xbb => println!("RES 7, E"),
                    //0xbc => println!("RES 7, H"),
                    //0xbd => println!("RES 7, L"),
                    //0xbe => println!("RES 7, (HL)"),
                    //0xbf => println!("RES 7, A"),
                    //0xc0 => println!("SET 0, B"),
                    //0xc1 => println!("SET 0, C"),
                    //0xc2 => println!("SET 0, D"),
                    //0xc3 => println!("SET 0, E"),
                    //0xc4 => println!("SET 0, H"),
                    //0xc5 => println!("SET 0, L"),
                    //0xc6 => println!("SET 0, (HL)"),
                    //0xc7 => println!("SET 0, A"),
                    //0xc8 => println!("SET 1, B"),
                    //0xc9 => println!("SET 1, C"),
                    //0xca => println!("SET 1, D"),
                    //0xcb => println!("SET 1, E"),
                    //0xcc => println!("SET 1, H"),
                    //0xcd => println!("SET 1, L"),
                    //0xce => println!("SET 1, (HL)"),
                    //0xcf => println!("SET 1, A"),
                    //0xd0 => println!("SET 2, B"),
                    //0xd1 => println!("SET 2, C"),
                    //0xd2 => println!("SET 2, D"),
                    //0xd3 => println!("SET 2, E"),
                    //0xd4 => println!("SET 2, H"),
                    //0xd5 => println!("SET 2, L"),
                    //0xd6 => println!("SET 2, (HL)"),
                    //0xd7 => println!("SET 2, A"),
                    //0xd8 => println!("SET 3, B"),
                    //0xd9 => println!("SET 3, C"),
                    //0xda => println!("SET 3, D"),
                    //0xdb => println!("SET 3, E"),
                    //0xdc => println!("SET 3, H"),
                    //0xdd => println!("SET 3, L"),
                    //0xde => println!("SET 3, (HL)"),
                    //0xdf => println!("SET 3, A"),
                    //0xe0 => println!("SET 4, B"),
                    //0xe1 => println!("SET 4, C"),
                    //0xe2 => println!("SET 4, D"),
                    //0xe3 => println!("SET 4, E"),
                    //0xe4 => println!("SET 4, H"),
                    //0xe5 => println!("SET 4, L"),
                    //0xe6 => println!("SET 4, (HL)"),
                    //0xe7 => println!("SET 4, A"),
                    //0xe8 => println!("SET 5, B"),
                    //0xe9 => println!("SET 5, C"),
                    //0xea => println!("SET 5, D"),
                    //0xeb => println!("SET 5, E"),
                    //0xec => println!("SET 5, H"),
                    //0xed => println!("SET 5, L"),
                    //0xee => println!("SET 5, (HL)"),
                    //0xef => println!("SET 5, A"),
                    //0xf0 => println!("SET 6, B"),
                    //0xf1 => println!("SET 6, C"),
                    //0xf2 => println!("SET 6, D"),
                    //0xf3 => println!("SET 6, E"),
                    //0xf4 => println!("SET 6, H"),
                    //0xf5 => println!("SET 6, L"),
                    //0xf6 => println!("SET 6, (HL)"),
                    //0xf7 => println!("SET 6, A"),
                    //0xf8 => println!("SET 7, B"),
                    //0xf9 => println!("SET 7, C"),
                    //0xfa => println!("SET 7, D"),
                    //0xfb => println!("SET 7, E"),
                    //0xfc => println!("SET 7, H"),
                    //0xfd => println!("SET 7, L"),
                    //0xfe => println!("SET 7, (HL)"),
                    //0xff => println!("SET 7, A"),
                    _ => println!("not an opcode"),
                }
            },

            _ => println!("not an opcode"),
        }


        if PC > 255
        {
            println!("End of boot ROM!!");
            break;
        }
    }
}