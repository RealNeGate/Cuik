// ON_DBG flags only show up when you're debugging, ON_REL are release flags, these stay.
//
//   ON_DBG/ON_REL(name, default_val)
//
ON_DBG(PASSES,           false)
ON_DBG(PEEP,             false)
ON_DBG(SCCP,             false)
ON_DBG(LOOP,             false)
ON_DBG(SROA,             false)
ON_DBG(GCM,              false)
ON_DBG(SLP,              false)
ON_DBG(INTERP,           false)
ON_DBG(MEMORY,           false)
ON_DBG(ISEL,             false)
ON_DBG(EMIT,             false)
ON_DBG(DATAFLOW,         false)
ON_DBG(PLACEMENT,        false)
ON_DBG(INLINE,           false)
ON_DBG(INLINE2,          false)
ON_DBG(INLINE3,          false)
ON_DBG(INLINE4,          false)
ON_DBG(REGSPLIT,         false)
ON_DBG(REGALLOC,         false)
ON_DBG(REGALLOC2,        false)
ON_DBG(REGALLOC3,        false)
ON_DBG(REGALLOC_AREA,    false)
ON_DBG(REGALLOC5,        false)
ON_DBG(REGALLOC6,        false)
ON_DBG(COMPACT,          false)
ON_DBG(SCHED1,           false)
ON_DBG(SCHED2,           false)
ON_DBG(SCHED3,           false)
ON_DBG(SCHED4,           false)
ON_DBG(SERVER,           false)
// for toggling ANSI colors
ON_DBG(ANSI,             true)

#undef ON_DBG
#undef ON_REL
