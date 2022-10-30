int f (int z) {
  if (z + sizeof (enum {a}))
    return 1 + sizeof (enum {a});
  return 0;
}
