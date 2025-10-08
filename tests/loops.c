#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#define pi 3.141592653589793
#define solar_mass (4 * pi * pi)
#define days_per_year 365.24

struct planet {
    double x, y, z;
    double vx, vy, vz;
    double mass;
};

#if 0
void advance(int nbodies, struct planet *bodies) {
    int i;
    for (i = 0; i < nbodies; i++) {
        struct planet *b = &(bodies[i]);
        b->x += b->vx;
        b->y += b->vy;
        b->z += b->vz;
    }
}
#else
void advance(int nbodies, struct planet *bodies) {
    int i, j;

    for (i = 0; i < nbodies; i++) {
        struct planet *b = &(bodies[i]);
        for (j = i + 1; j < nbodies; j++) {
            struct planet *b2 = &(bodies[j]);
            double dx = b->x - b2->x;
            double dy = b->y - b2->y;
            double dz = b->z - b2->z;
            double inv_distance = 1.0 / sqrt(dx * dx + dy * dy + dz * dz);
            double mag = inv_distance * inv_distance * inv_distance;
            b->vx -= dx * b2->mass * mag;
            b->vy -= dy * b2->mass * mag;
            b->vz -= dz * b2->mass * mag;
            b2->vx += dx * b->mass * mag;
            b2->vy += dy * b->mass * mag;
            b2->vz += dz * b->mass * mag;
        }
    }
}
#endif

