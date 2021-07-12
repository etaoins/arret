# Visual Studio Code currently uses Node 14.16
FROM node:14-buster

RUN \
  apt-get update && \
  apt-get -y install --no-install-recommends xvfb libnss3 libgtk-3-0 libxtst6 libxss1 libasound2 libsecret-1-0 libgbm1 && \
  apt-get clean

WORKDIR /workdir/editors/code

COPY package.json yarn.lock tsconfig.json ./
COPY src/test/ src/test/

RUN yarn install --frozen-lockfile
RUN yarn compile && yarn vscode:download
