
FROM haskell


RUN apt-get update && apt-get install -y \
    libpq-dev  

WORKDIR /app


COPY . .


RUN stack setup

RUN stack build --install-ghc


COPY . .

RUN stack build

# Expose the port that your application listens on (adjust if necessary)
EXPOSE 3000


CMD ["stack", "run"]
