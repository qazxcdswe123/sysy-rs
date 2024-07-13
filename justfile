koopa:
    docker run -it --rm -v ~/repos/mvll:/root/compiler compiler-dev autotest -koopa -s lv1 /root/compiler

riscv:
    docker run -it --rm -v ~/repos/mvll:/root/compiler compiler-dev autotest -riscv -s lv1 /root/compiler
    