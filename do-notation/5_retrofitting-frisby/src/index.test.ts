import { pipe } from 'fp-ts/lib/function';
import frisby from 'frisby';
import { expect, test } from 'vitest';
import { bind, Do, map } from './frisbySpec';

const getUrl = (id: number) => `https://jsonplaceholder.typicode.com/posts/${id}`
const getId = (res: frisby.FrisbyResponse): number => res.json.id

test('three consecutive calls', () => {
  return frisby
    .get(getUrl(1))
    .then(getId)
    .then(id1 =>
      frisby
        .get(getUrl(id1 + 1))
        .then(getId)
        .then(id2 =>
          frisby
            .get(getUrl(id1 + id2))
            .then(getId)
            .then(id3 =>
              frisby
                .get(getUrl(id1 + id2 + id3))
                .then(getId)
                .then(() => [id1, id2, id3])
                .then(result => expect(result).toEqual([1, 2, 3]))))
    )
});

test('three consecutive calls with do-notation', () => {
  return pipe(
    Do,
    bind("id1", () => frisby
      .get(getUrl(1))
      .then(getId)),
    bind("id2", ({ id1 }) => frisby
      .get(getUrl(id1 + 1))
      .then(getId)),
    bind("id3", ({ id1, id2 }) => frisby
      .get(getUrl(id1 + id2))
      .then(getId)),
    map(({ id1, id2, id3 }) => [id1, id2, id3]),
    spec => spec.then(result => expect(result).toEqual([1, 2, 3]))
  )
});