
local N = tonumber(arg[1])

print("void matmul(int* dst, int* a, int* b) {")
for i=0,(N*N) - 1 do
    print(string.format("    int a%d = a[%d];", i, i))
    print(string.format("    int b%d = b[%d];", i, i))
end
for i=0,N-1 do
    for j=0,N-1 do
        print(string.format("    int sum%d_%d = 0;", i, j))
        for k=0,N-1 do
            print(string.format("    sum%d_%d += a%d * b%d;", i, j, i*N + k, j*N + k))
        end
        print(string.format("    dst[%d] = sum%d_%d;", i*N + j, i, j))
    end
end
print("}")


