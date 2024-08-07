
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

# Set environment variables for database configuration
ENV export POSTGRES_HOST=localhost
ENV export POSTGRES_PORT=5432
ENV export POSTGRES_USER=ruchita
ENV export POSTGRES_PASS=qwerty
ENV export POSTGRES_DB=orville_project

CMD ["stack", "exec", "OrvilleProject-exe"]
