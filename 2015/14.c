#include <stdio.h>

#define MIN(a, b) ((a) < (b) ? (a) : (b))

typedef struct {
  char name[32];
  int speed;
  int fly_time;
  int rest_time;
} reindeer;

int reindeer_parse(reindeer *r, FILE *f) {
  int result = fscanf(f,
      "%s can fly %d km/s for %d seconds, but then must rest for %d seconds.",
      (char *) &r->name, &r->speed, &r->fly_time, &r->rest_time);
  return result == 4;
}

int reindeer_position(reindeer *r, int seconds) {
  int period_time = r->fly_time + r->rest_time;
  int period_distance = r->fly_time * r->speed;
  int num_periods = seconds / period_time;
  int remainder_time = seconds - num_periods * period_time;
  int remainder_distance = MIN(r->fly_time, remainder_time) * r->speed;
  return num_periods * period_distance + remainder_distance;
}

int reindeer_lead(reindeer *reindeer, int num_reindeer, int seconds) {
  int lead = 0;
  for(int i = 0; i < num_reindeer; i++) {
    int position = reindeer_position(reindeer + i, seconds);
    if(position > lead) lead = position;
  }
  return lead;
}

int main(int argc, char **argv) {
  /* Parse reindeer. */
  int total_seconds = 2503;
  reindeer reindeer[32];
  int points[32] = {0};
  int num_reindeer = 0;
  while(reindeer_parse(reindeer + num_reindeer, stdin)) {
      num_reindeer++;
  }

  /* Part 1. */
  printf("%d\n", reindeer_lead(reindeer, num_reindeer, total_seconds));

  /* Part 2. */
  for(int seconds = 1; seconds <= total_seconds; seconds++) {
    int lead = reindeer_lead(reindeer, num_reindeer, seconds);
    for(int i = 0; i < num_reindeer; i ++) {
      if(reindeer_position(reindeer + i, seconds) == lead) {
        points[i]++;
      }
    }
  }
  int best_points = 0;
  for(int i = 0; i < num_reindeer; i++) {
    if(points[i] > best_points) best_points = points[i];
  }
  printf("%d\n", best_points);

  return 0;
}
