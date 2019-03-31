FROM haskell:8
RUN mkdir -p /opt/build/
WORKDIR /opt/build/

COPY stack.yaml *.cabal ./
RUN export PATH=$(stack path --local-bin):$PATH
RUN stack build --resolver lts-12.10 --dependencies-only
COPY . /opt/build/
RUN stack install --resolver lts-12.10 pokerb
ENTRYPOINT ["pokerb"]
