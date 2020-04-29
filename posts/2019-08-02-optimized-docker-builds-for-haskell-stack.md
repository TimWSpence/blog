---
title: Optimized Docker builds for Haskell Stack
---

Stack recently [removed support for automatically building Docker images for
Haskell apps](https://github.com/commercialhaskell/stack/releases/tag/v2.1.1),
citing the reason that “with the advent of Docker multistage builds, this
functionality is no longer useful.”

However, I’ve yet to find a good resource on how to actually write a multistage
Docker build for a Stack-based Haskell application. I [posted an initial
solution on
r/Haskell](https://www.reddit.com/r/haskell/comments/cdiyd6/multistage_docker_builds_replacing_stack_image/)
but was rightly challenged that this solution requires we re-compile all
dependencies on every build, which is expensive in a CI environment.

So I went back to the drawing board to come up with a Docker build for Haskell applications which would use multistage builds to optimize both final image size and build time.

```Docker
# Loosely based on https://www.fpcomplete.com/blog/2017/12/building-haskell-apps-with-docker
FROM fpco/stack-build:lts-13.27 as dependencies
RUN mkdir /opt/build
WORKDIR /opt/build

# GHC dynamically links its compilation targets to lib gmp
RUN apt-get update \
  && apt-get download libgmp10
RUN mv libgmp*.deb libgmp.deb

# Docker build should not use cached layer if any of these is modified
COPY stack.yaml package.yaml stack.yaml.lock /opt/build/
RUN stack build --system-ghc --dependencies-only

# -------------------------------------------------------------------------------------------
FROM fpco/stack-build:lts-13.27 as build

# Copy compiled dependencies from previous stage
COPY --from=dependencies /root/.stack /root/.stack
COPY . /opt/build/

WORKDIR /opt/build

RUN stack build --system-ghc

RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin

# -------------------------------------------------------------------------------------------
# Base image for stack build so compiled artifact from previous
# stage should run
FROM ubuntu:16.04 as app
RUN mkdir -p /opt/app
WORKDIR /opt/app

# Install lib gmp
COPY --from=dependencies /opt/build/libgmp.deb /tmp
RUN dpkg -i /tmp/libgmp.deb && rm /tmp/libgmp.deb

COPY --from=build /opt/build/bin .
EXPOSE 8080
CMD ["/opt/app/app", "8080"]
```

```sh
docker pull ns/app-dependencies:latest || true

docker build --target dependencies --cache-from ns/app-dependencies:latest -t ns/app-dependencies .

docker build --target app --cache-from ns/app-dependencies:latest -t ns/app .

docker push ns/app-dependencies:latest
docker push ns/app:latest
```

We’ve defined 3 Docker images here:

* The first is responsible for fetching LibGMP (which GHC dynamically links its
  compilation targets to) and compiling dependencies. It should only need to be
  re-built if `stack.yaml` or `package.yaml` are modified. Fortunately, that is
  exactly the behaviour that Docker’s layer caching gives us.
* The second is where we build our application and it re-uses Stack’s cache of
  compiled dependencies from the first stage.
* Finally, the third image will be our compiled application. Note, that it’s
  based on Ubuntu rather than `fpco/stack-build` and hence is much more
  lightweight. For one of our applications, the dependencies image was 10.3GB
  whereas the final app image was just 142MB!

This all works because of the build script, which uses Docker’s —-cache-from
feature to re-use layers from the dependencies stage. If you find that CI builds
are still too slow (you are downloading a ~10GB image after all), you may want
to `docker save` your dependencies image to a persistent shared folder if the
filesystem is faster than the network—although I couldn’t possibly condone such
a hacky solution. 😜 Also, watch out for concurrent builds if you do this: you
might need to rely on atomic renames or similar.

In fact, downloading the images was so slow that we switched to using the
official `haskell:8.6.5` as a base image instead of `fpco/stack-build`. However,
this uses an older version of Stack and doesn’t seem to be updated as frequently
so I wouldn’t necessarily recommend it unless the build times are too painful
for you. If you do switch to this, remember to update the base image for your
final stage to be the same as the base image for haskell (`debian:stretch` instead
of `ubuntu:16.04`) as well to ensure your app is compiled against the correct
version of glibc, etc

### Caveat Emptor

Note that this solution exhibits degenerate behaviour in the case where a change
has modified `{stack,package}.yaml` and there is at least one other change being
built concurrently. In this case, the cache of compiled dependencies will be
wiped out on every build as Docker sees that these files have been modified. The
best solution I’ve come up with if you need to modify the stack manifests is to
make a small commit directly to master which only modifies these files. This
should mean that that commit is a common ancestor of any work in progress and
hence they all agree on the contents of `{stack,package}.yaml`
