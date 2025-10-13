// Quick smoke test for memory initialization optimization
import './output/_init.mjs';

console.log('Testing optimized memory initialization...');
const start = Date.now();

// Create CPU instance (tests memory init)
const cpu = new abap.Classes['ZCL_CPU_8080_V2']();
await cpu.constructor_();

const elapsed = Date.now() - start;
console.log(`✓ CPU created in ${elapsed}ms`);

if (elapsed < 5000) {
  console.log('✅ SUCCESS: Memory initialization is FAST! (< 5 seconds)');
  console.log(`   16 self-concatenations >> 65536 individual concatenations`);
  process.exit(0);
} else {
  console.log(`⚠️  Memory initialization took ${elapsed}ms (still slow, needs more optimization)`);
  process.exit(1);
}
