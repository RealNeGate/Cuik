
local N = tonumber(arg[1])

print("void matmul(float* dst, float* a, float* b) {")
for i=0,(N*N) - 1 do
    print(string.format("    float a%d = a[%d];", i, i))
    print(string.format("    float b%d = b[%d];", i, i))
end
for i=0,N-1 do
    for j=0,N-1 do
        print(string.format("    float sum%d_%d = -0.0f;", i, j))
        for k=0,N-1 do
            print(string.format("    sum%d_%d += a%d * b%d;", i, j, i*N + k, j*N + k))
        end
        print(string.format("    dst[%d] = sum%d_%d;", i*N + j, i, j))
    end
end
print("}")


