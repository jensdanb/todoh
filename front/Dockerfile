ARG NODE_VERSION=24
FROM node:${NODE_VERSION}-alpine

WORKDIR /app

COPY package.json .
RUN npm i

COPY . .
RUN npm run build

ENV ORIGIN=https://todo.jensdanbolt.no

# use if not on same machine and docker network as backend. IE; not part of the same "docker compose up". 
# for dev on same machione but outside docker compose, use http://localhost/api
# ENV BACKEND_URL=

EXPOSE 3000

CMD node build
# CMD npm run dev
