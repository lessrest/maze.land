build:; docker-compose build
migrate-maze:; docker-compose run -T maze-api rake db:migrate
create-maze:; docker-compose run -T maze-api rake db:create
api-console:; docker-compose exec maze-api rails c
