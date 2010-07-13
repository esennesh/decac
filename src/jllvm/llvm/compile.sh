gcc -c Core_wrap.c -I/usr/lib/jvm/java-6-openjdk/include -I/usr/lib/jvm/java-6-openjdk/include/linux `llvm-config --cflags --libs core`
gcc -shared Core_wrap.o -o Core.so `llvm-config --cflags --libs core`

gcc -c ExecutionEngine_wrap.c -I/usr/lib/jvm/java-6-openjdk/include -I/usr/lib/jvm/java-6-openjdk/include/linux `llvm-config --cflags --libs ExecutionEngine`
gcc -shared ExecutionEngine_wrap.o -o ExecutionEngine.so `llvm-config --cflags --libs ExecutionEngine`

gcc -c LinkTimeOptimizer_wrap.c -I/usr/lib/jvm/java-6-openjdk/include -I/usr/lib/jvm/java-6-openjdk/include/linux `llvm-config --cflags --libs Linker`
gcc -shared LinkTimeOptimizer_wrap.o -o LinkTimeOptimizer.so `llvm-config --cflags --libs Linker`

gcc -c lto_wrap.c -I/usr/lib/jvm/java-6-openjdk/include -I/usr/lib/jvm/java-6-openjdk/include/linux `llvm-config --cflags --libs Linker`
gcc -shared lto_wrap.o -o lto.so `llvm-config --cflags --libs Linker`

gcc -c Analysis_wrap.c -I/usr/lib/jvm/java-6-openjdk/include -I/usr/lib/jvm/java-6-openjdk/include/linux `llvm-config --cflags --libs Analysis`
gcc -shared Analysis_wrap.o -o Analysis.so `llvm-config --cflags --libs Analysis`

gcc -c BitReader_wrap.c -I/usr/lib/jvm/java-6-openjdk/include -I/usr/lib/jvm/java-6-openjdk/include/linux `llvm-config --cflags --libs BitReader`
gcc -shared BitReader_wrap.o -o BitReader.so `llvm-config --cflags --libs BitReader`

gcc -c BitWriter_wrap.c -I/usr/lib/jvm/java-6-openjdk/include -I/usr/lib/jvm/java-6-openjdk/include/linux `llvm-config --cflags --libs BitWriter`
gcc -shared BitWriter_wrap.o -o BitWriter.so `llvm-config --cflags --libs BitWriter`

gcc -c Target_wrap.c -I/usr/lib/jvm/java-6-openjdk/include -I/usr/lib/jvm/java-6-openjdk/include/linux `llvm-config --cflags --libs Target`
gcc -shared Target_wrap.o -o Target.so `llvm-config --cflags --libs Target`

gcc -c Transforms/Scalar_wrap.c -o Transforms/Scalar_wrap.o -I/usr/lib/jvm/java-6-openjdk/include -I/usr/lib/jvm/java-6-openjdk/include/linux `llvm-config --cflags --libs TransformUtils`
gcc -shared Transforms/Scalar_wrap.o -o Transforms/Scalar.so `llvm-config --cflags --libs TransformUtils`
