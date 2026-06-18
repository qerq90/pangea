FROM sbt:1.10.11-jdk21 AS builder
WORKDIR /build

COPY build.sbt .
COPY project/build.properties project/
COPY project/plugins.sbt project/
COPY project/Dependencies.scala project/
RUN sbt update

COPY . .
RUN sbt app/assembly


FROM eclipse-temurin:21-jre-jammy
WORKDIR /app

RUN apt-get update && apt-get install -y curl && \
    curl -fsSL https://github.com/pressly/goose/releases/download/v3.24.3/goose_linux_x86_64 \
      -o /usr/local/bin/goose && \
    chmod +x /usr/local/bin/goose && \
    apt-get purge -y curl && apt-get autoremove -y && rm -rf /var/lib/apt/lists/*

COPY --from=builder /build/app/target/scala-2.13/app.jar app.jar
COPY migrations/ migrations/
COPY entrypoint.sh .
RUN chmod +x entrypoint.sh

EXPOSE 8080
ENTRYPOINT ["./entrypoint.sh"]
