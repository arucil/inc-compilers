#![allow(clippy::unusual_byte_groupings)]
#![cfg(test)]

use serial_test::serial;
use std::ffi::c_void;

extern "C" {
  fn rt_initialize(rootstack_size: u64, heap_size: u64) -> *mut u64;
  fn rt_allocate(size: u64, rootstack_ptr: *const u64) -> *const c_void;
  fn rt_collect(rootstack_ptr: *const u64);
  fn rt_new_string(
    size: u64,
    chars: *const u8,
    rootstack_ptr: *const u64,
  ) -> *const c_void;
  fn rt_from_space() -> *const c_void;
  fn rt_heap_size() -> u64;
}

#[test]
#[serial]
fn initialize() {
  unsafe {
    rt_initialize(65536, 64);
    assert_eq!(rt_heap_size(), 64);
  }
}

#[test]
#[serial]
fn new_string() {
  unsafe {
    let rootstack_ptr = rt_initialize(65536, 65536);

    const STR1: &str = "abcde";
    let s = rt_new_string(STR1.len() as u64, STR1.as_ptr(), rootstack_ptr)
      as *const u64;
    assert_eq!(*s, 0b101_011);
    assert_eq!(*s.add(1) & 0xff_ffff_ffff, 0x6564636261);

    const STR2: &str = "abcdefghijk";
    let s = rt_new_string(STR2.len() as u64, STR2.as_ptr(), rootstack_ptr)
      as *const u64;
    assert_eq!(*s, 0b1011_011);
    assert_eq!(*s.add(1), 0x6867666564636261);
    assert_eq!(*s.add(2) & 0xffffff, 0x6b6a69);
  }
}

#[test]
#[serial]
fn extend_heap() {
  unsafe {
    let rootstack_ptr = rt_initialize(65536, 64);

    const STR1: &str = "abcde";
    let s = rt_new_string(STR1.len() as u64, STR1.as_ptr(), rootstack_ptr)
      as *const u64;

    let v = rt_allocate(5 * 8, rootstack_ptr) as *mut u64;
    *v = 0b0101_000100_001;
    *v.add(1) = s as u64;
    *v.add(2) = 100;
    *v.add(3) = v as u64;
    *v.add(4) = 1000;

    let old_heap = rt_from_space();

    let _v2 = rt_allocate(512, rootstack_ptr) as *mut u64;

    let heap = rt_from_space() as *const u64;
    assert_eq!(*heap, 0b101_011);
    assert_eq!(*heap.add(1) & 0xff_ffff_ffff, 0x6564636261);
    assert_eq!(*heap.add(2), 0b0101_000100_001);
    assert_eq!(
      *heap.add(3),
      heap as u64,
      "old heap: {:p}, new heap: {:p}, ptr: {:016x}",
      old_heap,
      heap,
      *heap.add(3)
    );
    assert_eq!(*heap.add(4), 100);
    assert_eq!(*heap.add(5), heap.add(2) as u64);
    assert_eq!(*heap.add(6), 1000);
  }
}

#[test]
#[serial]
fn collect() {
  unsafe {
    let rootstack_ptr = rt_initialize(65536, 64) as *mut u64;

    const STR1: &str = "abcde";
    let s = rt_new_string(STR1.len() as u64, STR1.as_ptr(), rootstack_ptr)
      as *const u64;

    let v = rt_allocate(5 * 8, rootstack_ptr) as *mut u64;
    *v = 0b0101_000100_001;
    *v.add(1) = s as u64;
    *v.add(2) = 100;
    *v.add(3) = v as u64;
    *v.add(4) = 1000;

    let old_heap = rt_from_space();

    *rootstack_ptr = v as u64;

    rt_collect(rootstack_ptr.add(1));

    let heap = rt_from_space() as *const u64;
    assert_eq!(*heap, 0b0101_000100_001);
    assert_eq!(
      *heap.add(1),
      heap.add(5) as u64,
      "old heap: {:p}, new heap: {:p}, ptr: {:016x}",
      old_heap,
      heap,
      *heap.add(1)
    );
    assert_eq!(*heap.add(2), 100);
    assert_eq!(*heap.add(3), heap as u64);
    assert_eq!(*heap.add(4), 1000);
    assert_eq!(*heap.add(5), 0b101_011);
    assert_eq!(*heap.add(6) & 0xff_ffff_ffff, 0x6564636261);

    *rootstack_ptr = heap.add(5) as u64;

    rt_collect(rootstack_ptr.add(1));

    const STR2: &str = "abcdefghijk";
    rt_new_string(STR2.len() as u64, STR2.as_ptr(), rootstack_ptr);

    let heap = rt_from_space() as *const u64;
    assert_eq!(*heap, 0b101_011);
    assert_eq!(*heap.add(1) & 0xff_ffff_ffff, 0x6564636261);
    assert_eq!(*heap.add(2), 0b1011_011);
    assert_eq!(*heap.add(3), 0x6867666564636261);
    assert_eq!(*heap.add(4) & 0xffffff, 0x6b6a69);
  }
}

#[test]
#[serial]
fn example_in_book() {
  unsafe {
    let rootstack_ptr = rt_initialize(65536, 65536) as *mut u64;

    // v7: [ 5 ]
    let v7 = rt_allocate(2 * 8, rootstack_ptr) as *mut u64;
    *v7 = 0b0_000001_001;
    *v7.add(1) = 8;

    // v1: [ #t | 42 ]
    let v1 = rt_allocate(3 * 8, rootstack_ptr) as *mut u64;
    *v1 = 0b00_000010_001;
    *v1.add(1) = 1;
    *v1.add(2) = 42;

    // v2: [ 3 | v1 ]
    let v2 = rt_allocate(3 * 8, rootstack_ptr) as *mut u64;
    *v2 = 0b10_000010_001;
    *v2.add(1) = 3;
    *v2.add(2) = v1 as u64;

    // v3: [ 7 | 5 | v2 ]
    let v3 = rt_allocate(4 * 8, rootstack_ptr) as *mut u64;
    *v3 = 0b100_000011_001;
    *v3.add(1) = 7;
    *v3.add(2) = 5;
    *v3.add(3) = v2 as u64;

    // v9: [ v10 | 6 ]
    let v9 = rt_allocate(3 * 8, rootstack_ptr) as *mut u64;
    *v9 = 0b01_000010_001;
    *v9.add(2) = 6;

    // v8: [ v7 ]
    let v8 = rt_allocate(2 * 8, rootstack_ptr) as *mut u64;
    *v8 = 0b1_000001_001;
    *v8.add(1) = v7 as u64;

    // v4: [ v3 | v1 ]
    let v4 = rt_allocate(3 * 8, rootstack_ptr) as *mut u64;
    *v4 = 0b11_000010_001;
    *v4.add(1) = v3 as u64;
    *v4.add(2) = v1 as u64;

    // v10: [ 2 | v9 ]
    let v10 = rt_allocate(3 * 8, rootstack_ptr) as *mut u64;
    *v10 = 0b10_000010_001;
    *v10.add(1) = 2;
    *v10.add(2) = v9 as u64;

    *v9.add(1) = v10 as u64;

    // v5: [ 8 ]
    let v5 = rt_allocate(2 * 8, rootstack_ptr) as *mut u64;
    *v5 = 0b0_000001_001;
    *v5.add(1) = 8;

    // v6: [ v5 | 4 ]
    let v6 = rt_allocate(3 * 8, rootstack_ptr) as *mut u64;
    *v6 = 0b01_000010_001;
    *v6.add(1) = v5 as u64;
    *v6.add(2) = 4;

    *rootstack_ptr = v4 as u64;
    *rootstack_ptr.add(1) = v3 as u64;
    *rootstack_ptr.add(2) = v6 as u64;

    rt_collect(rootstack_ptr.add(3));

    const STR1: &str = "abcde";
    rt_new_string(STR1.len() as u64, STR1.as_ptr(), rootstack_ptr);

    let heap = rt_from_space() as *const u64;
    assert_eq!(*heap, 0b11_000010_001);
    assert_eq!(*heap.add(1), heap.add(3) as u64);
    assert_eq!(*heap.add(2), heap.add(10) as u64);
    assert_eq!(*heap.add(3), 0b100_000011_001);
    assert_eq!(*heap.add(4), 7);
    assert_eq!(*heap.add(5), 5);
    assert_eq!(*heap.add(6), heap.add(13) as u64);
    assert_eq!(*heap.add(7), 0b01_000010_001);
    assert_eq!(*heap.add(8), heap.add(16) as u64);
    assert_eq!(*heap.add(9), 4);
    assert_eq!(*heap.add(10), 0b00_000010_001);
    assert_eq!(*heap.add(11), 1);
    assert_eq!(*heap.add(12), 42);
    assert_eq!(*heap.add(13), 0b10_000010_001);
    assert_eq!(*heap.add(14), 3);
    assert_eq!(*heap.add(15), heap.add(10) as u64);
    assert_eq!(*heap.add(16), 0b0_000001_001);
    assert_eq!(*heap.add(17), 8);
    assert_eq!(*heap.add(18), 0b101_011);
    assert_eq!(*heap.add(19) & 0xff_ffff_ffff, 0x6564636261);
  }
}