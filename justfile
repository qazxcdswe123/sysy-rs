cwd := `pwd`

koopa level:
    docker run -it --rm -v {{cwd}}:/root/compiler compiler-dev /bin/bash -c "autotest -koopa -s {{level}} /root/compiler"

riscv level:
    docker run -it --rm -v {{cwd}}:/root/compiler compiler-dev /bin/bash -c "autotest -riscv -s {{level}} /root/compiler"

k:
    cargo run -- -koopa hello.c -o hello.koopa

r:
    cargo run -- -riscv hello.c -o hello.riscv