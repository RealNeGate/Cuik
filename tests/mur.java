import sun.misc.Unsafe;
import java.lang.reflect.Field;
import java.nio.file.*;
import java.io.*;

class mur {

    public static Unsafe getUnsafe() {
        try {
            final Field fld = Unsafe.class.getDeclaredField("theUnsafe");
            fld.setAccessible(true);
            return (Unsafe) fld.get(mur.class);
        } catch (Exception e) {
            throw new RuntimeException("Could not obtain access to sun.misc.Unsafe", e);
        }
    }

    // unsafe stuff for loading 32bits from a byte array
    static final Unsafe U = getUnsafe();
    static final int ABASE = U.arrayBaseOffset(byte[].class);
    static int read32(byte data[], int i) { return U.getInt(data, ABASE + i); }

    static int murmur3_32(byte data[]) throws IOException {
        int h = 0;

        // main body, work on 32-bit blocks at a time
        for (int i=0; i<data.length/4; i++) {
            int k = read32(data, i*4);

            k *= 0xcc9e2d51;
            k = ((k << 15) | (k >>> 17))*0x1b873593;
            h = (((h^k) << 13) | ((h^k) >>> 19))*5 + 0xe6546b64;
        }

        // load/mix up to 3 remaining tail bytes into a tail block
        int t = 0;
        int tail = 4*(data.length/4);
        switch (data.length & 3) {
            case 3: t ^= data[tail+2] << 16;
            case 2: t ^= data[tail+1] <<  8;
            case 1: {
                t ^= data[tail+0] <<  0;
                h ^= ((0xcc9e2d51*t << 15) | (0xcc9e2d51*t >>> 17))*0x1b873593;
            }
        }

        // finalization mix, including key length
        h = ((h^data.length) ^ ((h^data.length) >>> 16))*0x85ebca6b;
        h = (h ^ (h >>> 13))*0xc2b2ae35;
        return (h ^ (h >>> 16));
    }

    public static void main(String[] args) throws IOException {
        byte[] data = Files.readAllBytes(Paths.get("W:/Workspace/Cuik/tests/test5.c"));
        System.out.printf("Wack! %#x\n", murmur3_32(data));
    }

}
