ARG NODE_VERSION=24
FROM node:${NODE_VERSION}-alpine

WORKDIR /app

COPY package.json .
RUN npm i

COPY . .
RUN npm run build

ENV ORIGIN=http://localhost
EXPOSE 3000

CMD node build
# CMD npm run dev
