FROM eclipse-temurin:21-jdk-jammy AS builder
WORKDIR /build

RUN apt-get update && apt-get install -y curl gnupg apt-transport-https && \
    echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list && \
    curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add - && \
    apt-get update && apt-get install -y sbt && \
    rm -rf /var/lib/apt/lists/*

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
