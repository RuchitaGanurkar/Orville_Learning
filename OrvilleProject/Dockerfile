FROM ubuntu:22.04


COPY ./ .

RUN sudo apt-get update \
    sudo apt-get upgrade \
    stack setup \
    stack build \
    sudo apt curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh     
    

EXPOSE 3000 


COPY .stack-work/install/x86_64-linux/9426f635756deea4d1c1a15610c4d68acba70ff55dc117e9cf2bba60ad1d0bee/9.6.6/bin/   .


CMD ["stack build", "stack run"]



