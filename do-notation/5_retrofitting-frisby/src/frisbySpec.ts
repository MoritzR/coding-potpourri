
import { bind as bind_, Chain1 } from 'fp-ts/lib/Chain';
import frisby, { FrisbySpec } from 'frisby';

const URI = 'FrisbySpec'
type URI = typeof URI
declare module 'fp-ts/HKT' {
    interface URItoKind<A> {
        readonly FrisbySpec: FrisbySpec<A>
    }
}

const FrisbyChain: Chain1<URI> = {
    URI,
    map: <A, B>(ma: FrisbySpec<A>, f: (a: A) => B) =>
        ma.then(f) as FrisbySpec<B>,
    ap: <A, B>(fab: FrisbySpec<(a: A) => B>, fa: FrisbySpec<A>) =>
        fab.then(f => fa.then(a => f(a))) as FrisbySpec<B>,
    chain: (fa, f) => fa.then(f)
}

export const Do = frisby.fromJSON({}).then(res => res.json as {})
export const bind = bind_(FrisbyChain)
export const map = <A, B>(ab: (a: A) => B) => (fa: FrisbySpec<A>) =>
    FrisbyChain.map(fa, ab)