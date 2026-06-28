------------------------------------------------------------------------

editor_options: markdown: wrap: 72 ---

# Dependence Is Not Destiny: How Positions in the Global Digital Trade Network Shape Digital Entrepreneurial Ecosystems

*Working structure and draft for submission to* **Research Policy** *(article type: full research paper). Placeholders for tables, figures and results are marked* `[TABLE X]`, `[FIGURE X]` *and* `[RESULT]`.

------------------------------------------------------------------------

## Abstract *(placeholder — draft last, \~200 words)*

We examine how a country's *position* in the global digital trade network shapes its digital entrepreneurial ecosystem. Using bilateral digital-trade data for [N] countries over [2016–2021], we characterise each country on five dimensions of digital trade structure — trade dependency, supplier concentration (HHI), digital openness, a network-based **Digital Cycling Index (DCI)** adapted from the Finn cycling index, and **network brokerage** (betweenness) — and relate them to four *distinct* pillars of the digital entrepreneurship ecosystem (**DTE, DMSP, DUC, DTI**) rather than a single composite. Moving beyond ranking exposed countries, we ask *when* dependency is harmful: we test whether **network embeddedness and brokerage moderate dependency**, so that well-positioned ecosystems absorb external reliance that would otherwise be damaging. [RESULT headline — e.g. dependency matters less than network position; effects concentrate in the innovation and market-support pillars]. The findings reframe digital-sovereignty debates from "reduce dependence" toward "improve network position," with direct implications for European strategic autonomy.

**Keywords:** digital dependency; digital entrepreneurial ecosystems; digital sovereignty; production networks; Finn cycling index; network brokerage; betweenness centrality; strategic autonomy

------------------------------------------------------------------------

## 1. Introduction *(placeholder — \~1,200 words)*

Frame the puzzle and contribution. Suggested arc:

1.  Digital technologies have become general-purpose inputs into entrepreneurship; access to cloud, platforms, enterprise software and digital infrastructure now conditions who can start and scale firms.
2.  This access is overwhelmingly *imported* and *concentrated* in a handful of foreign providers, raising sovereignty and resilience concerns (esp. for the EU).
3.  The dominant policy framing equates dependency with vulnerability. We argue this is incomplete: what matters is not only *how much* a country relies on outsiders but *how embedded* it is in reciprocal, capability-building digital networks.
4.  **Contribution:** (i) a network adaptation of the Finn cycling index to digital trade (the DCI); (ii) a four-dimension characterisation of digital trade structure across the world; (iii) evidence that embeddedness *moderates* the dependency–ecosystem relationship; (iv) a policy typology of countries.
5.  Roadmap.

State the three research questions and the headline moderation hypothesis explicitly here.

------------------------------------------------------------------------

## 2. Digitalisation, Entrepreneurship and Entrepreneurial Ecosystems

*(Lit section 1. \~1,500 words. Establishes the dependent-variable side.)*

**2.1 Digitalisation as a general-purpose transformation.** How digital technologies lower entry barriers, reshape opportunity discovery, and change the unit of competitive advantage.

**2.2 From entrepreneurship to entrepreneurial ecosystems.** Why the ecosystem (not the firm) is the right unit; the DEE construct and its components (digital infrastructure, platforms, users, entrepreneurial activity, ecosystem support).

**2.3 Why digital inputs are ecosystem foundations.** Cloud/infrastructure/platforms as the "soil" of digital entrepreneurship; complementarities and lock-in.

*Literature to engage:*

-   Spigel (2017); Stam (2015); Stam & van de Ven (2021) — entrepreneurial ecosystems.
-   Autio et al. (2018); Nambisan (2017); Sussan & Acs (2017) — **digital** entrepreneurship and the digital ecosystem.
-   Bharadwaj et al. (2013); Yoo et al. (2010) — digital infrastructure / generativity.
-   Bresnahan & Trajtenberg (1995) — general-purpose technologies.
-   Cennamo & Santaló; Cusumano, Gawer & Yoffie (2019) — platforms as ecosystem hubs.
-   Acs, Autio & Szerb (2014) — National Systems of Entrepreneurship (measurement lineage for DEE).

------------------------------------------------------------------------

## 3. Digital Dependency: Why External Reliance Is Dangerous — and When It Is Not

*(Lit section 2 + positioning. \~2,000 words.)*

**3.1 Trade dependency and the economics of reliance.** Classic arguments: import dependence, exposure to supply shocks, terms-of-trade and bargaining vulnerability.

**3.2 Concentration risk.** Why dependence concentrated in few suppliers is qualitatively worse (single points of failure, coercion, "weaponised interdependence").

**3.3 From trade dependency to *digital* dependency.** What makes digital different: non-substitutable infrastructure, switching costs, data and standards lock-in, extraterritorial control (cloud, OS, payment rails, cybersecurity), and the speed at which access can be revoked. Digital sovereignty literature.

**3.4 The missing dimension: embeddedness.** Dependency and concentration answer "how exposed?" and "how concentrated?". They are silent on whether a country *participates in* the value-creation network. We introduce embeddedness (DCI) as the third axis and argue it conditions the harm of dependency.

**3.5 What we do here.** State the four indicators, the network adaptation, and the three hypotheses.

*Hypotheses:*

-   **H1.** Higher digital trade dependency is associated with weaker ecosystem pillars.
-   **H2.** Higher supplier concentration (HHI) is associated with weaker pillars.
-   **H3a.** Higher network embeddedness (DCI) is associated with *stronger* pillars, conditional on dependency and concentration.
-   **H3b.** Higher network brokerage (betweenness) is associated with *stronger* pillars (gateway advantage).
-   **H4 (headline moderation).** Embeddedness and brokerage **moderate** dependency: the negative association between dependency and ecosystem strength is attenuated (or reversed) where DCI/brokerage are high — *dependence is not destiny.*
-   **H5 (mechanism / heterogeneity).** Network structure shapes some pillars more than others: we expect the strongest effects on the **innovation (DTE)** and **market-support / platform (DMSP)** pillars, and weaker effects on **user-adoption conditions (DUC)**.

*Literature to engage:*

-   Hirschman (1945/1980); Farrell & Newman (2019) — weaponised interdependence.
-   Baldwin (2016) — global value chains and the nature of 21st-century trade.
-   Crémer, de Montjoye & Schweitzer (2019); EU digital sovereignty / strategic autonomy reports.
-   Hidalgo & Hausmann (2009) — economic complexity / capability embeddedness (conceptual cousin of DCI).
-   Gereffi, Humphrey & Sturgeon (2005) — governance of value chains.

------------------------------------------------------------------------

## 4. Data and Methods

*(\~3,000 words. Where the four indicators and the network construction live.)*

### 4.1 Data

-   **Digital trade.** Bilateral, predicted digital-services trade flows (exporter → importer), [HQ-based] specification, [2016–2021], [N] countries, across [K] technology categories mapped to four **technology groups**: *infrastructure, platform, productive, user-facing* (mapping in Appendix A).
-   **Digital Entrepreneurial Ecosystem (DEE).** Index and its sub-components, [source/year].
-   **Macro controls.** GDP and GDP per capita (World Bank WDI); [population, internet users] as available.

State sample, coverage, and the reference year used for the cross-sectional tables.

### 4.2 The four dimensions of digital trade structure

For importer *i*, technology *t*, year *y*, with bilateral flow $t_{ji}$ (from supplier *j* to *i*):

**(a) Trade dependency**

$$\text{Dependency}_{i} = \frac{M_i}{M_i + X_i}$$

where $M_i$ and $X_i$ are total digital imports and exports. Ranges 0–1; higher = more reliant on foreign supply. *Policy question: "How exposed are we?"*

**(b) Supplier concentration (Herfindahl–Hirschman Index)**

$$\text{HHI}_{i} = \sum_j s_{ji}^2,\qquad s_{ji} = \frac{t_{ji}}{\sum_k t_{ki}}$$

Higher = supply concentrated in few partners. *Policy question: "How concentrated is that exposure?"*

**(c) Digital openness**

$$\text{Openness}_{i} = \frac{M_i + X_i}{\text{GDP}_i}$$

Total digital trade integration relative to economic size. *Policy question: "How connected are we?"*

**(d) Digital Cycling Index (DCI) — a network adaptation of the Finn Cycling Index.**

We treat countries as nodes and bilateral digital flows as the transaction matrix $Z$, with $Z_{ij}$ the flow from *i* to *j*. Following Finn (1976) and Braun et al. (2021):

$$a_{ij} = \frac{Z_{ij}}{x_j}, \qquad L = (I - A)^{-1}, \qquad \hat{l}_i = \frac{l_{ii}-1}{l_{ii}}$$

$$\text{FCI} = \frac{1}{\sum_i x_i}\sum_i \hat{l}_i\, x_i$$

where \$x_j = \$ total digital **throughput** of *j* (exports + imports), used as the total-output proxy. Because the column sums of $A$ equal $\text{imports}_j/(\text{imports}_j+\text{exports}_j) < 1$, $(I-A)$ is invertible. The **economy-wide FCI** is a single scalar; the **country-level DCI is the node cycling term** $\hat{l}_i$ — country *i*'s share of throughput involved in feedback loops. *Policy question: "Do we participate in ecosystem-level, recursive value creation?"*

We compute DCI three ways: **Global DCI** (full world network), **per-technology-group DCI**, and **European DCI** (EU-27 subnetwork only).

*On the diagonal.* Bilateral trade is cross-border by construction ($Z_{ii}=0$), so the baseline DCI measures **multilateral round-trip embeddedness** ($i\to j\to\dots\to i$). Where a domestic self-supply estimate is available on the same scale as the flows (e.g. $Z_{ii}=\text{domestic market}_i-\text{imports}_i$, or $\text{domestic production}_i-\text{exports}_i$, by technology), the diagonal can be populated. Doing so shifts the construct toward **domestic digital self-sufficiency** (self-loops are the strongest cycles) — a sovereignty reading rather than a network-embeddedness one. We report the cross-border DCI as the baseline and the diagonal-augmented DCI as a [robustness check / alternative construct], stating the choice explicitly. *(The estimation code supports both via an optional `domestic` argument.)*

**(e) Network brokerage (betweenness centrality).**

We treat the directed digital-trade network and compute, per country and year, weighted betweenness centrality — the share of shortest digital-trade paths passing through a country (edge "distance" = 1/flow, so stronger ties are shorter hops; normalised). High brokerage marks **gateway economies** — intermediaries through which digital value flows (candidates: Ireland, the Netherlands, Singapore). Computed on the global network (**Brokerage**) and the EU-27 subnetwork (**Brokerage_EU**). Brokerage is conceptually distinct from DCI: a country can broker flows between others (high betweenness) without being embedded in feedback loops that return value to itself (DCI).

**Interpretation summary**

| Metric | Measures | Policy question |
|----|----|----|
| Dependency | Reliance on foreign inputs | How exposed are we? |
| HHI | Supplier concentration | How concentrated is the exposure? |
| Openness | Total trade integration | How connected are we? |
| DCI | Position in recursive value loops | Do we participate in capability creation? |
| Brokerage | Gateway position (betweenness) | Does value flow *through* us? |

### 4.3 Extra-European Dependency Ratio (EEDR)

For EU country *i*:

$$\text{EEDR}_i = \frac{\sum_{j \in \text{Non-EU}} t_{ji}}{\sum_{k} t_{ki}}$$

The share of digital imports sourced from outside the EU. Paired with the European DCI, it distinguishes external reliance from intra-European embeddedness.

### 4.4 Technology Vulnerability Index (TVI) — *robustness device, not the main contribution*

For each country × technology group we min-max scale the four dimensions to [0,1], **invert** the protective dimensions (DCI always; openness by default), and average:

$$\text{TVI} = \tfrac{1}{4}\big( \tilde{D} + \widetilde{\text{HHI}} + (1-\widetilde{\text{Open}}) + (1-\widetilde{\text{DCI}}) \big)$$

Countries are classified into **Low / Medium / High / Very High** by sample quartiles. We keep the four dimensions separate in the main models and report the TVI only as a summary visualisation, pre-empting the "why these weights?" critique.

### 4.5 Empirical strategy

We use the four ecosystem pillars (**DTE, DMSP, DUC, DTI**) as *separate* outcomes — not just the aggregate DEE — which lets us identify *which dimension* of the ecosystem network structure affects (pre-empting the reviewer question "which dimension is actually affected?"). Panel of country-years with country and year fixed effects; SE clustered by country (`fixest::feols`). Framed as **structural associations**, not causal identification (§7).

**Block A — all-country main models (one per outcome).** $$\text{Pillar}_{it} = \beta_1\text{Dep} + \beta_2\text{HHI} + \beta_3\text{Open} + \beta_4\text{DCI}^{G} + \beta_5\text{Brokerage}^{G} + \gamma\log\text{GDPpc} + \mu_i + \tau_t$$ estimated for DTE, DMSP, DUC, DTI (and aggregate DEE for reference).

**Block B — moderation (the headline).** - (B1) $\text{Pillar} = \beta_1\text{Dep} + \beta_2\text{DCI}^{G} + \beta_3(\text{Dep}\times\text{DCI}^{G})$ — embeddedness moderates dependency. - (B2) $\text{Pillar} = \beta_1\text{Dep} + \beta_2\text{Brokerage}^{G} + \beta_3(\text{Dep}\times\text{Brokerage}^{G})$ — gateway position moderates dependency.

**Block C — EU strategic-autonomy models (EU sample, one per outcome).** $$\text{Pillar} = \beta_1\text{EEDR} + \beta_2\text{HHI} + \beta_3\text{DCI}^{EU} + \beta_4\text{Brokerage}^{EU} + \gamma\log\text{GDPpc} + \mu_i + \tau_t$$ plus the EU interaction $\text{Dep}\times\text{DCI}^{EU}$.

**Block D — dynamic / temporal precedence.** Re-estimate Block A with the *lead* of each outcome ($\text{Pillar}_{i,t+1}$ on network structure at $t$), moving toward "network position precedes performance".

**Mechanism read (H5).** Compare coefficient patterns across pillars; the memorable result would be that network structure (DCI, brokerage) shapes **DTE/DMSP** but not **DUC** — "digital trade networks primarily shape the innovation and market-support dimensions of ecosystems, not user-adoption conditions."

Controls: log GDP per capita throughout.

### 4.6 Country typology (cluster analysis)

Country means of Dependency and DCI are split at their medians into four named policy quadrants, cross-checked with a standardised four-cluster k-means on all five dimensions (Dependency, DCI, Brokerage, HHI, Openness):

|                     | Low DCI                    | High DCI              |
|---------------------|----------------------------|-----------------------|
| **Low dependency**  | **Sovereign but Isolated** | **Embedded Leaders**  |
| **High dependency** | **Vulnerable Periphery**   | **Gateway Economies** |

We additionally examine the **Brokerage × Dependency** plane (Figure 4): high-brokerage, high-dependency countries are *gateway economies* that may thrive *despite* dependency — the clearest expression of "dependence is not destiny."

------------------------------------------------------------------------

## 5. Results

### 5.1 Descriptive: the global map of digital trade structure

`[TABLE 1 — Country master dataset: Dependency, HHI, Openness, Global DCI, Brokerage, DTE, DMSP, DUC, DTI]` Narrative: distribution of the five structural dimensions, leaders/laggards, and a correlation matrix showing the dimensions and pillars are *not* redundant. `[FIGURE 1 — Conceptual framework: Global Digital Trade Network → {Dependency, HHI, DCI, Brokerage} → DEE pillars {DTE, DMSP, DUC, DTI}]` `[FIGURE 2 — The global digital trade network: node size = DEE, node colour = Dependency]`

### 5.2 Technology-group vulnerability

`[TABLE 2 — TVI by technology group: country × {user-facing, productive, platform, infrastructure}]` `[TABLE 3 — TVI classes (Low/Medium/High/Very High) by technology group]` Narrative: which technology layers drive vulnerability; infrastructure vs user-facing contrast.

### 5.3 The European strategic-dependency picture

`[TABLE 4 — EU countries: Dependency, EEDR, HHI, European DCI, Brokerage_EU, DTE, DMSP, DUC, DTI]` Narrative: high EEDR but heterogeneous embeddedness/brokerage; set up the moderation story. (Likely the table policymakers read first.)

### 5.4 Main models — which pillar is affected? (Block A)

`[RESULT — all-country models, one column per outcome: DEE, DTE, DMSP, DUC, DTI]` Walk through H1–H3b across pillars; highlight the mechanism (H5): where do DCI and Brokerage load, and is DUC the exception?

### 5.5 Moderation — dependence is not destiny (Block B)

`[RESULT — Dependency×DCI and Dependency×Brokerage interactions, per pillar]` Report interaction signs; plot the marginal effect of dependency across the range of DCI and of brokerage. This is the headline.

### 5.6 EU strategic-autonomy models (Block C)

`[RESULT — EU sample, EEDR/HHI/DCI_EU/Brokerage_EU per pillar + EU interaction]`

### 5.7 Dynamic / temporal precedence (Block D)

`[RESULT — lead-outcome models: network_t → pillar_{t+1}]`

### 5.8 Country typology and figures

`[RESULT — quadrant_summary and kmeans_summary]` `[FIGURE 3 — Dependency vs DCI quadrants: Embedded Leaders / Gateway Economies / Sovereign but Isolated / Vulnerable Periphery (point size = DEE)]` `[FIGURE 4 — Brokerage vs Dependency: gateway economies (e.g. Ireland, Netherlands, Singapore) in the upper-right]` Contrast a "Gateway Economy" (high dependency, high brokerage, strong ecosystem) with a "Vulnerable Periphery" case.

### 5.9 Robustness

`[RESULT — legacy DEE-component models; TVI openness-direction sensitivity; diagonal-augmented DCI; OT vs HQ trade specification]`

------------------------------------------------------------------------

## 6. Discussion *(\~1,500 words)*

-   Reframe sovereignty: embeddedness, not just dependency, governs resilience.
-   The moderation result as the core theoretical contribution (dependency is contingent, not uniformly harmful).
-   Technology-layer heterogeneity (infrastructure dependence is the binding constraint).
-   Policy: from "reduce imports" to "build reciprocal European digital value loops" (DCI as a target).

## 7. Limitations and Threats to Validity *(addresses reviewer attacks up front)*

-   **Construct circularity ("explaining one index with others").** Mitigated by using DEE sub-components as DVs (§5.5) and keeping the four trade dimensions separate.
-   **Endogeneity / reverse causality.** Strong ecosystems may trade more digitally. We claim structural association, not causal identification; FE absorb time-invariant confounders; discuss instrument/lagged-X options as future work.
-   **DCI and the domestic diagonal.** Bilateral data has no within-country digital production, so the baseline DCI captures *international* round-trip embeddedness, not domestic recirculation; throughput is used as the output proxy. The diagonal can be populated with a domestic self-supply estimate (changing the construct toward self-sufficiency); we report this as a [robustness check] and are explicit about which construct each result uses.
-   **Predicted (modelled) trade flows.** Discuss measurement error and robustness to the OT vs HQ specification.
-   **TVI weighting.** Equal weights are a normative choice; reported only as robustness, with openness-direction sensitivity.

## 8. Conclusion *(placeholder)*

------------------------------------------------------------------------

## Appendices

-   **Appendix A.** Technology category → group mapping (infrastructure / platform / productive / user-facing).
-   **Appendix B.** Country coverage and ISO3 reconciliation.
-   **Appendix C.** Full descriptive statistics, correlation matrix, and evolution plots (2016–2021).
-   **Appendix D.** Robustness tables (OT specification, alternative TVI weights, k-means diagnostics).

## References *(to be completed)*

Finn, J.T. (1976) *Measuring the processing of energy and matter in ecosystems.* — DCI/FCI origin. Braun et al. (2021) *The strength of domestic production networks.* — FCI applied to economies (uploaded). [Resilience / ENA paper, 1-s2.0-S0954349X24001656] — structural resilience metrics (uploaded). Plus the entrepreneurship-ecosystem, digital-entrepreneurship, GVC, and digital-sovereignty references listed in §§2–3.
