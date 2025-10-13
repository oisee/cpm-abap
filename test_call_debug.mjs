import './output/init.mjs';

async function test() {
  console.log("Creating CPU...");
  const cpu = await (new abap.Classes['ZCL_CPU_8080_V2']()).constructor_();

  console.log("Resetting CPU...");
  await cpu.reset();

  console.log("Initial state:");
  console.log("  PC:", await cpu.get_pc());
  console.log("  SP:", await cpu.get_sp());
  console.log("  Status:", await cpu.get_status());

  // Write test program
  console.log("\nWriting test program:");
  console.log("  256: 0x31 (LD SP,nnnn)");
  await cpu.write_byte({iv_addr: abap.IntegerFactory.get(256), iv_val: abap.IntegerFactory.get(49)});
  console.log("  257: 0xFF");
  await cpu.write_byte({iv_addr: abap.IntegerFactory.get(257), iv_val: abap.IntegerFactory.get(255)});
  console.log("  258: 0xFF");
  await cpu.write_byte({iv_addr: abap.IntegerFactory.get(258), iv_val: abap.IntegerFactory.get(255)});
  console.log("  259: 0xCD (CALL nnnn)");
  await cpu.write_byte({iv_addr: abap.IntegerFactory.get(259), iv_val: abap.IntegerFactory.get(205)});
  console.log("  260: 0x00");
  await cpu.write_byte({iv_addr: abap.IntegerFactory.get(260), iv_val: abap.IntegerFactory.get(0)});
  console.log("  261: 0x30");
  await cpu.write_byte({iv_addr: abap.IntegerFactory.get(261), iv_val: abap.IntegerFactory.get(48)});

  // Execute first instruction (LD SP,nnnn)
  console.log("\nExecuting instruction 1 (LD SP,nnnn)...");
  await cpu.execute_instruction();
  console.log("After LD SP:");
  console.log("  PC:", await cpu.get_pc());
  console.log("  SP:", await cpu.get_sp());
  console.log("  Status:", await cpu.get_status());

  // Execute second instruction (CALL nnnn)
  console.log("\nExecuting instruction 2 (CALL nnnn)...");
  await cpu.execute_instruction();
  console.log("After CALL:");
  console.log("  PC:", await cpu.get_pc());
  console.log("  SP:", await cpu.get_sp());
  console.log("  Status:", await cpu.get_status());

  console.log("\nExpected:");
  console.log("  PC: 12288 (0x3000)");
  console.log("  SP: 65533 (0xFFFD)");
}

test().then(() => {
  console.log("\nTest complete");
  process.exit(0);
}).catch((err) => {
  console.error("Test failed:", err);
  process.exit(1);
});
