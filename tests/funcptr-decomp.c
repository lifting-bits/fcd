void _init(uint64_t rip)
{
	if (*(uint64_t*)0x600ff8 != 0)
	{
		__gmon_start__(4195357);
	}
	return;
}
void strchr(uint64_t rip)
{
	__indirect_jump(*(uint64_t*)0x601018);
}
void printf(uint64_t rip)
{
	__indirect_jump(*(uint64_t*)0x601020);
}
void __libc_start_main(uint64_t rip)
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
	__libc_start_main(4195497);
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
	uint8_t* anon1 = (uint8_t*)0x601040;
	if (*anon1 == 0)
	{
		deregister_tm_clones(4195650);
		*anon1 = 1;
	}
	return;
}
void frame_dummy(uint64_t rip)
{
	return;
}
void cm_to_inches(uint64_t rip)
{
	(__asm "movsd qword ptr [rbp - 8], xmm0")();
	(__asm "movsd xmm0, qword ptr [rbp - 8]")();
	(__asm "movsd xmm1, qword ptr [rip + 0x10c]")();
	(__asm "divsd xmm0, xmm1")();
	return;
}
void main(uint64_t rip)
{
	((uint64_t(*)(uint64_t, uint64_t))0x400440)(4195976, 112);
	(__asm "movsd xmm0, qword ptr [rbp - 0x28]")();
	((void(*)())0x400576)();
	printf(4195817);
	return;
}
void __libc_csu_init(uint64_t rip, uint64_t rdi, uint64_t rsi, uint64_t rdx)
{
	_init(4195889);
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
