#include <stdint.h>
#include <stddef.h>

typedef struct Dope {
    int offset;
    int limit;
    char* data;
} Dope;

void NNN_matmul_square_i8_i32_4(size_t N, Dope* dst, Dope* a, Dope* b) {
    uint8_t* a_arr   = (uint8_t*) &a->data[a->offset];
    uint8_t* b_arr   = (uint8_t*) &b->data[b->offset];
    uint32_t* restrict dst_arr = (uint32_t*) &dst->data[dst->offset];

    for (size_t kk = 0; kk < N; kk += 4) {
        for (size_t jj = 0; jj < N; jj += 4) {
            for (size_t i = 0; i < N; i++) {
                // A = 4x1
                uint32_t a0 = a_arr[i*N + (kk+0)];
                uint32_t a1 = a_arr[i*N + (kk+1)];
                uint32_t a2 = a_arr[i*N + (kk+2)];
                uint32_t a3 = a_arr[i*N + (kk+3)];
                // B = 4x4
                uint32_t b0 = b_arr[(kk+0)*N + (jj+0)];
                uint32_t b1 = b_arr[(kk+1)*N + (jj+0)];
                uint32_t b2 = b_arr[(kk+2)*N + (jj+0)];
                uint32_t b3 = b_arr[(kk+3)*N + (jj+0)];
                uint32_t b4 = b_arr[(kk+0)*N + (jj+1)];
                uint32_t b5 = b_arr[(kk+1)*N + (jj+1)];
                uint32_t b6 = b_arr[(kk+2)*N + (jj+1)];
                uint32_t b7 = b_arr[(kk+3)*N + (jj+1)];
                uint32_t b8 = b_arr[(kk+0)*N + (jj+2)];
                uint32_t b9 = b_arr[(kk+1)*N + (jj+2)];
                uint32_t b10 = b_arr[(kk+2)*N + (jj+2)];
                uint32_t b11 = b_arr[(kk+3)*N + (jj+2)];
                uint32_t b12 = b_arr[(kk+0)*N + (jj+3)];
                uint32_t b13 = b_arr[(kk+1)*N + (jj+3)];
                uint32_t b14 = b_arr[(kk+2)*N + (jj+3)];
                uint32_t b15 = b_arr[(kk+3)*N + (jj+3)];
                // C' += A * B
                dst_arr[i*N + (jj+0)] += a0*b0 + a1*b1 + a2*b2 + a3*b3;
                dst_arr[i*N + (jj+1)] += a0*b4 + a1*b5 + a2*b6 + a3*b7;
                dst_arr[i*N + (jj+2)] += a0*b8 + a1*b9 + a2*b10 + a3*b11;
                dst_arr[i*N + (jj+3)] += a0*b12 + a1*b13 + a2*b14 + a3*b15;
            }
        }
    }
}
