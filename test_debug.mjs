// Debug test to see what's failing
import './output/_init.mjs';

console.log('Creating CPU...');
const cpu = new abap.Classes['ZCL_CPU_8080_V2']();
await cpu.constructor_();

console.log('Resetting CPU...');
await cpu.reset();

console.log('Writing byte to memory...');
await cpu.write_byte({iv_addr: 256, iv_val: 0});  // NOP

console.log('Executing NOP instruction...');
await cpu.execute_instruction();

console.log('Getting PC...');
const pc = await cpu.get_pc();
console.log(`PC after NOP: ${pc.get()}`);

if (pc.get() === 257) {
  console.log('✅ NOP test passed!');
} else {
  console.log(`❌ NOP test failed: expected 257, got ${pc.get()}`);
}
