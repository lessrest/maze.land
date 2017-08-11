FROM node:8.3.0-alpine
WORKDIR /app
RUN npm install formidable aws-sdk
ADD index.js /app/index.js
CMD ["node", "index.js"]
