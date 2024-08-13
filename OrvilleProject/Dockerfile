
FROM haskell

RUN apt-get update && apt-get install -y \
  libpq-dev  # PostgreSQL client library

WORKDIR /app

# COPY stack.yaml stack.yaml
# COPY stack.yaml.lock stack.yaml.lock

# COPY package.yaml package.yaml

# COPY src/ src/
# COPY app/ app/

COPY . .

RUN stack setup
RUN stack build

# stack build --system-ghc

COPY . .

RUN stack build

# Expose the port that your application listens on (adjust if necessary)
EXPOSE 3000

# Set environment variables for database configuration
ENV POSTGRES_HOST=localhost
ENV POSTGRES_PORT=5432
ENV POSTGRES_USER=ruchita
ENV POSTGRES_PASS=qwerty
ENV POSTGRES_DB=orville_project

CMD ["stack", "exec", "OrvilleProject-exe"]
