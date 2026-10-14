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

# Касса Т-Банка (securepay.tinkoff.ru) отдаёт сертификат, подписанный корневым
# CA Минцифры — «Russian Trusted Root CA». В eclipse-temurin его нет, и без него
# первый же запрос к кассе падает на PKIX: «unable to find valid certification
# path to requested target», то есть донат не работает вовсе.
#
# Сертификат лежит в репозитории, а не качается при сборке: так образ собирается
# одинаково и не зависит от доступности gu-st.ru. Отпечаток сверяется здесь же —
# подменённый файл обрушит сборку, а не добавит молча чужой корень в доверенные.
# Он совпадает с тем, что отдаёт сама касса в своей цепочке, и с публикацией
# Минцифры на https://gu-st.ru/content/Other/doc/russian_trusted_root_ca.cer
ARG RU_ROOT_SHA256=D2:6D:2D:02:31:B7:C3:9F:92:CC:73:85:12:BA:54:10:35:19:E4:40:5D:68:B5:BD:70:3E:97:88:CA:8E:CF:31
COPY docker/russian_trusted_root_ca.cer /tmp/ru-root.cer
RUN keytool -printcert -file /tmp/ru-root.cer | grep -q "SHA256: ${RU_ROOT_SHA256}" && \
    keytool -importcert -noprompt -trustcacerts -cacerts -storepass changeit \
      -alias russian-trusted-root-ca -file /tmp/ru-root.cer && \
    rm /tmp/ru-root.cer

COPY --from=builder /build/app/target/scala-2.13/app.jar app.jar
COPY migrations/ migrations/
COPY entrypoint.sh .
RUN chmod +x entrypoint.sh

EXPOSE 8080
ENTRYPOINT ["./entrypoint.sh"]
