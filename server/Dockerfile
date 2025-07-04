
FROM haskell:9.10.1-bullseye AS build_stage

WORKDIR /haskell_build

RUN cabal update

# Add just the .cabal file to capture dependencies. Source -> Destination
COPY ./todoh.cabal todoh.cabal
RUN cabal build --only-dependencies -j4

# Add and Install Application Code
COPY . .
RUN cabal build
# Executable built at: /dist-newstyle/build/x86_64-linux/ghc-9.10.1/todoh-0.1.0.0/x/todoh/build/todoh/todoh


FROM haskell:9.10.1-bullseye AS run_stage

WORKDIR /app

COPY --from=build_stage ./haskell_build/dist-newstyle/build/x86_64-linux/ghc-9.10.1/todoh-0.1.0.0/x/todoh/build/todoh/todoh /todoh

EXPOSE 8080
CMD ["/todoh"]
