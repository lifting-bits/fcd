void _init(uint64_t rip)
{
	if (*(uint64_t*)0x600ff8 != 0)
	{
		__gmon_start__(4195389);
	}
	return;
}
void __stack_chk_fail(uint64_t rip)
{
	__indirect_jump(*(uint64_t*)0x601018);
}
void __libc_start_main(uint64_t rip)
{
	__indirect_jump(*(uint64_t*)0x601020);
}
void atoi(uint64_t rip)
{
	__indirect_jump(*(uint64_t*)0x601028);
}
void __gmon_start__(uint64_t rip)
{
	__indirect_jump(*(uint64_t*)0x600ff8);
}
void _start(uint64_t rip, uint64_t rsp, uint64_t rdx)
{
	*(uint64_t*)(rsp - 8) = rip;
	uint64_t anon1 = rsp & 0xfffffffffffffff0;
	*(uint64_t*)(anon1 - 16) = anon1 - 8;
	__libc_start_main(4195529);
	__builtin_trap();
}
void deregister_tm_clones(uint64_t rip)
{
	return;
}
void register_tm_clones(uint64_t rip)
{
	return;
}
void __do_global_dtors_aux(uint64_t rip)
{
	uint8_t* anon1 = (uint8_t*)0x601044;
	if (*anon1 == 0)
	{
		deregister_tm_clones(4195682);
		*anon1 = 1;
	}
	return;
}
void frame_dummy(uint64_t rip)
{
	return;
}
uint64_t f(uint64_t rip, uint64_t rdi)
{
	atoi(4195763);
	atoi(4195777);
	return 0;
}
void main(uint64_t rip)
{
	struct { uint8_t field0[7]; uint8_t field1; uint64_t field2; uint8_t field3[8]; uint64_t field4; } alloca1;
	alloca1.field4 = rip;
	alloca1.field2 = *__fs_ptr_i64(40);
	alloca1.field1 = 66;
	*(uint32_t*)0x601048 = (uint32_t)f(4195832, (uint64_t)&alloca1.field1);
	if (*__fs_ptr_i64(40) != alloca1.field2)
	{
		__stack_chk_fail(4195864);
	}
	return;
}
void __libc_csu_init(uint64_t rip, uint64_t rdi, uint64_t rsi, uint64_t rdx)
{
	_init(4195921);
	(*(void(**)(uint64_t, uint64_t, uint64_t))0x600e10)(rdi & 0xffffffff, rsi, rdx);
	return;
}
void __libc_csu_fini(uint64_t rip)
{
	return;
}
void _fini(uint64_t rip)
{
	return;
}
