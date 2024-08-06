
FROM haskell


WORKDIR /app

COPY stack.yaml stack.yaml
COPY stack.yaml.lock stack.yaml.lock

COPY . .


RUN apt-get update && apt-get install -y \
  libpq-dev  # PostgreSQL client library


RUN stack setup
RUN stack build --install-ghc

# stack build --system-ghc



# Expose the port that your application listens on (adjust if necessary)
EXPOSE 3000


CMD ["stack", "exec", "OrvilleProject-exe"]
