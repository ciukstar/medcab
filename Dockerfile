FROM ubuntu:22.04
RUN mkdir -p /opt/medcab \
	&& apt-get update \
	&& apt-get install -y --no-install-recommends build-essential zlib1g-dev libpq-dev libicu-dev \
	&& apt-get install -y ca-certificates && update-ca-certificates \
	&& apt-get clean \
	&& rm -rf /var/lib/apt/lists/*

ARG YESOD_SUPERUSER_USERNAME
ARG YESOD_SUPERUSER_PASSWORD
ARG YESOD_DEMO_LANG=EN
ARG YESOD_GOOGLE_CLIENT_ID
ARG YESOD_GOOGLE_CLIENT_SECRET

WORKDIR /opt/medcab
COPY medcab /opt/medcab
COPY static /opt/medcab/static
COPY config /opt/medcab/config

ENV YESOD_PORT=8080
ENV YESOD_DEMO_LANG=${YESOD_DEMO_LANG}
ENV YESOD_SUPERUSER_USERNAME=${YESOD_SUPERUSER_USERNAME}
ENV YESOD_SUPERUSER_PASSWORD=${YESOD_SUPERUSER_PASSWORD}
ENV YESOD_GOOGLE_CLIENT_ID=${YESOD_GOOGLE_CLIENT_ID}
ENV YESOD_GOOGLE_CLIENT_SECRET=${YESOD_GOOGLE_CLIENT_SECRET}

EXPOSE 8080
CMD ["./medcab"]
