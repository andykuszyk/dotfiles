FROM ubuntu
ENV TZ=Europe/London
RUN ln -snf /usr/share/zoneinfo/$TZ /etc/localtime && echo $TZ > /etc/timezone
WORKDIR /dotfiles
COPY ./ .
RUN apt update && apt install make sudo && make install-packages
RUN useradd --create-home andy && usermod -aG sudo andy
USER andy
RUN make new-machine
CMD zsh
