#include <emmintrin.h>

typedef struct TriangleX4 {
	__m128 ax, ay, az;
	__m128 bx, by, bz;
	__m128 cx, cy, cz;
} TriangleX4;

typedef struct TriangleNormalX4 {
	__m128 nx, ny, nz;
} TriangleNormalX4;

// Computes normals 4-wide
void gen_normals(const TriangleX4* tri, TriangleNormalX4* out) {
    // a.y * b.z - a.z * b.y
    __m128 nx = _mm_sub_ps(_mm_mul_ps(tri->ay, tri->bz), _mm_mul_ps(tri->az, tri->by));
	
    // a.z * b.x - a.x * b.z
    __m128 ny = _mm_sub_ps(_mm_mul_ps(tri->az, tri->bx), _mm_mul_ps(tri->ax, tri->bz));
	
    // a.x * b.y - a.y * b.x
    __m128 nz = _mm_sub_ps(_mm_mul_ps(tri->ax, tri->by), _mm_mul_ps(tri->ay, tri->bx));
	
    // 1.0 / sqrt((nx * nx) + (ny * ny) + (nz * nz))
    __m128 inv_length = _mm_rsqrt_ps(_mm_add_ps(_mm_add_ps(_mm_mul_ps(nx, nx), _mm_mul_ps(ny, ny)), _mm_mul_ps(nz, nz)));
	
    // NOTE(NeGate): This will shit out NaN if the 
    // triangle is a degenerate fuck
    out->nx = _mm_mul_ps(nx, inv_length);
    out->ny = _mm_mul_ps(ny, inv_length);
    out->nz = _mm_mul_ps(nz, inv_length);
}

