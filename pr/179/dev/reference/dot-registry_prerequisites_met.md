# Can a Domain Run Through the Registry?

Registry generators assume their upstream domains were themselves built
by the registry (site -\> subject -\> everything else; `Raw_STUDY` is
not registered). When an upstream domain came from a fallback tier
(sparse custom specs), the registry generator cannot run, so the domain
falls through to later tiers. Failures with all prerequisites in place
are real errors and are not masked.

## Usage

``` r
.registry_prerequisites_met(domain, registry_domains)
```
