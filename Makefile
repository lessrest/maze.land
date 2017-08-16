build:; docker-compose build
migrate-maze:; docker-compose run -T maze-api rake db:migrate
create-maze:; docker-compose run -T maze-api rake db:create
api-console:; docker-compose exec maze-api rails c
update-lambdas:; make -C lambdas/transcode
delete-clips:; \
aws s3 rm --recursive s3://restless-videos/original && \
aws s3 rm --recursive s3://restless-videos/clip
