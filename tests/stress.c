#include <stdio.h>
#include <stdint.h>
static uint32_t mur1(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur2(h, k);
}
static uint32_t mur2(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur3(h, k);
}
static uint32_t mur3(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur4(h, k);
}
static uint32_t mur4(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur5(h, k);
}
static uint32_t mur5(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur6(h, k);
}
static uint32_t mur6(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur7(h, k);
}
static uint32_t mur7(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur8(h, k);
}
static uint32_t mur8(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur9(h, k);
}
static uint32_t mur9(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur10(h, k);
}
static uint32_t mur10(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur11(h, k);
}
static uint32_t mur11(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur12(h, k);
}
static uint32_t mur12(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur13(h, k);
}
static uint32_t mur13(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur14(h, k);
}
static uint32_t mur14(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur15(h, k);
}
static uint32_t mur15(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur16(h, k);
}
static uint32_t mur16(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur17(h, k);
}
static uint32_t mur17(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur18(h, k);
}
static uint32_t mur18(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur19(h, k);
}
static uint32_t mur19(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur20(h, k);
}
static uint32_t mur20(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur21(h, k);
}
static uint32_t mur21(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur22(h, k);
}
static uint32_t mur22(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur23(h, k);
}
static uint32_t mur23(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur24(h, k);
}
static uint32_t mur24(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur25(h, k);
}
static uint32_t mur25(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur26(h, k);
}
static uint32_t mur26(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur27(h, k);
}
static uint32_t mur27(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur28(h, k);
}
static uint32_t mur28(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur29(h, k);
}
static uint32_t mur29(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur30(h, k);
}
static uint32_t mur30(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur31(h, k);
}
static uint32_t mur31(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur32(h, k);
}
static uint32_t mur32(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur33(h, k);
}
static uint32_t mur33(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur34(h, k);
}
static uint32_t mur34(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur35(h, k);
}
static uint32_t mur35(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur36(h, k);
}
static uint32_t mur36(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur37(h, k);
}
static uint32_t mur37(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur38(h, k);
}
static uint32_t mur38(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur39(h, k);
}
static uint32_t mur39(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur40(h, k);
}
static uint32_t mur40(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur41(h, k);
}
static uint32_t mur41(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur42(h, k);
}
static uint32_t mur42(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur43(h, k);
}
static uint32_t mur43(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur44(h, k);
}
static uint32_t mur44(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur45(h, k);
}
static uint32_t mur45(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur46(h, k);
}
static uint32_t mur46(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur47(h, k);
}
static uint32_t mur47(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur48(h, k);
}
static uint32_t mur48(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur49(h, k);
}
static uint32_t mur49(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur50(h, k);
}
static uint32_t mur50(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur51(h, k);
}
static uint32_t mur51(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur52(h, k);
}
static uint32_t mur52(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur53(h, k);
}
static uint32_t mur53(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur54(h, k);
}
static uint32_t mur54(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur55(h, k);
}
static uint32_t mur55(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur56(h, k);
}
static uint32_t mur56(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur57(h, k);
}
static uint32_t mur57(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur58(h, k);
}
static uint32_t mur58(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur59(h, k);
}
static uint32_t mur59(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur60(h, k);
}
static uint32_t mur60(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur61(h, k);
}
static uint32_t mur61(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur62(h, k);
}
static uint32_t mur62(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur63(h, k);
}
static uint32_t mur63(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur64(h, k);
}
static uint32_t mur64(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur65(h, k);
}
static uint32_t mur65(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur66(h, k);
}
static uint32_t mur66(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur67(h, k);
}
static uint32_t mur67(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur68(h, k);
}
static uint32_t mur68(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur69(h, k);
}
static uint32_t mur69(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur70(h, k);
}
static uint32_t mur70(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur71(h, k);
}
static uint32_t mur71(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur72(h, k);
}
static uint32_t mur72(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur73(h, k);
}
static uint32_t mur73(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur74(h, k);
}
static uint32_t mur74(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur75(h, k);
}
static uint32_t mur75(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur76(h, k);
}
static uint32_t mur76(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur77(h, k);
}
static uint32_t mur77(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur78(h, k);
}
static uint32_t mur78(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur79(h, k);
}
static uint32_t mur79(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur80(h, k);
}
static uint32_t mur80(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur81(h, k);
}
static uint32_t mur81(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur82(h, k);
}
static uint32_t mur82(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur83(h, k);
}
static uint32_t mur83(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur84(h, k);
}
static uint32_t mur84(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur85(h, k);
}
static uint32_t mur85(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur86(h, k);
}
static uint32_t mur86(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur87(h, k);
}
static uint32_t mur87(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur88(h, k);
}
static uint32_t mur88(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur89(h, k);
}
static uint32_t mur89(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur90(h, k);
}
static uint32_t mur90(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur91(h, k);
}
static uint32_t mur91(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur92(h, k);
}
static uint32_t mur92(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur93(h, k);
}
static uint32_t mur93(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur94(h, k);
}
static uint32_t mur94(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur95(h, k);
}
static uint32_t mur95(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur96(h, k);
}
static uint32_t mur96(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur97(h, k);
}
static uint32_t mur97(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur98(h, k);
}
static uint32_t mur98(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur99(h, k);
}
static uint32_t mur99(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur100(h, k);
}
static uint32_t mur100(uint32_t h, uint32_t k) {
    k *= 0xcc9e2d51;
    k = ((k << 15) | (k >> 17))*0x1b873593;
    h = (((h^k) << 13) | ((h^k) >> 19))*5 + 0xe6546b64;
    return mur101(h, k);
}
static int mur101(uint32_t h, uint32_t k) { return h; }
int main() {
    printf("Hash: %#x", mur1(0, 1));
    return 0;
}
