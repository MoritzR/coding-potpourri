import { pipe } from 'fp-ts/lib/function';
import frisby from 'frisby';
import { expect, test } from 'vitest';
import { bind, Do, map } from './frisbySpec';

const getUrl = (id: number) => `https://jsonplaceholder.typicode.com/posts/${id}`
const getId = (res: frisby.FrisbyResponse): number => res.json.id
const getIdFromUrl = (id: number) => frisby.get(getUrl(id)).then(getId)

test('three consecutive calls', () => {
  return getIdFromUrl(1)
    .then(id1 =>
      getIdFromUrl(id1 + 1)
        .then(id2 =>
          getIdFromUrl(id1 + id2)
            .then(id3 =>
              getIdFromUrl(id1 + id2 + id3)
                .then(() => [id1, id2, id3])
                .then(result => expect(result).toEqual([1, 2, 3]))))
    )
});

test('three consecutive calls with do-notation', () => {
  return pipe(
    Do,
    bind("id1", () => getIdFromUrl(1)),
    bind("id2", ({ id1 }) => getIdFromUrl(id1 + 1)),
    bind("id3", ({ id1, id2 }) => getIdFromUrl(id1 + id2)),
    map(({ id1, id2, id3 }) => [id1, id2, id3]),
    spec => spec.then(result => expect(result).toEqual([1, 2, 3]))
  )
});