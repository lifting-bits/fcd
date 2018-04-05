void _init(uint64_t rip)
{
	if (*(uint64_t*)0x600ff8 != 0)
	{
		__gmon_start__(4195293);
	}
	return;
}
void __libc_start_main(uint64_t rip)
{
	__indirect_jump(*(uint64_t*)0x601018);
}
void strtol(uint64_t rip)
{
	__indirect_jump(*(uint64_t*)0x601020);
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
	__libc_start_main(4195417);
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
	uint8_t* anon1 = (uint8_t*)0x601038;
	if (*anon1 == 0)
	{
		deregister_tm_clones(4195570);
		*anon1 = 1;
	}
	return;
}
void frame_dummy(uint64_t rip)
{
	return;
}
void f(uint64_t rip)
{
	strtol(4195641);
	return;
}
void main(uint64_t rip)
{
	strtol(4195641);
	return;
}
void __libc_csu_init(uint64_t rip, uint64_t rdi, uint64_t rsi, uint64_t rdx)
{
	_init(4195713);
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
