koopa:
    docker run -it --rm -v ~/repos/mvll:/root/compiler compiler-dev /bin/bash -c "autotest -koopa -s lv3 /root/compiler"

riscv:
    docker run -it --rm -v ~/repos/mvll:/root/compiler compiler-dev /bin/bash -c "autotest -riscv -s lv3 /root/compiler"