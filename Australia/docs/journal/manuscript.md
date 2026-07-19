# What One Aggregate Equation Can Identify About Credit-Conditioned Consumption

### A single-equation LIVES estimate for Australia, 1976–2024

---

## Abstract

We ask how much of the Muellbauer–Williams credit-conditioned consumption model — in which the propensity to consume out of housing wealth is switched on and off by the state of credit — can be identified from a single aggregate time-series equation for Australia. Assembling a household-sector dataset extended back to 1976 and estimating the consumption function in its faithful form, in a conventional constant-propensity form, and in a form that imposes the original Australian calibration, we separate what the data support from what they cannot pin down. A bounds test confirms a genuine level relationship between consumption and its long-run determinants for both estimated forms, carried jointly by wealth, housing, credit conditions and permanent income rather than by any imposed consumption-to-income restriction, which is itself non-stationary. The faithful functional form, not the longer sample, sharpens the estimated speed of adjustment, and the institutionally-timed credit-conditions index carries real identifying content relative to a random-knot placebo. Yet once the uncertainty in constructing the credit index and the permanent-income measure is propagated through a nested bootstrap, only the sign and non-zero magnitude of the error-correction speed survive: every individual wealth, permanent-income and credit-interaction elasticity is statistically indistinguishable from zero, none can reject the original calibration, and the permanent-income channel even reverses sign under a real-time measure. A single credit-conditioned equation can identify that Australian consumption error-corrects to a credit-sensitive long-run relation; it cannot identify the magnitudes of the individual channels.

**Keywords:** consumption; housing wealth; credit conditions; error correction; permanent income; identification

**JEL:** E21, E44, C22, C52

---

## 1. Introduction

Australian household consumption sits at the centre of a set of questions the representative-agent workhorse is poorly equipped to answer. How sensitive is consumption to housing wealth at different points in the credit cycle, when households can or cannot extract home equity? How much of the post-2008 moderation reflects macroprudential tightening rather than balance-sheet repair? How should a central bank think about the wealth channel of monetary policy when most household wealth is housing, mortgage debt is near historic highs relative to income, and the credit environment has shifted repeatedly since deregulation in the 1980s?

The framework designed to answer these questions is the Muellbauer–Williams "latent interactive variable equation system", in the flow-of-funds tradition of Tobin and Dolde (1971) and of Duca and Muellbauer (2013). It augments the credit-augmented life-cycle consumption function (Friedman 1957; Ando and Modigliani 1963; Modigliani 1963) with three features. Wealth is disaggregated into net liquid assets, illiquid financial assets, and housing assets, each entered as a ratio to income and each carrying its own marginal propensity to consume (Backus and Purvis 1980). A latent credit-conditions index interacts with the long-run relationship so that key channels activate only as credit eases. And — decisively for what follows — the same latent index is identified *jointly* across a four-equation system (consumption, house prices, mortgage stock, home-equity withdrawal) under cross-equation restrictions estimated by full-information maximum likelihood. Williams (2010) applied this system to Australia over 1978–2008, producing the canonical Australian estimate; sixteen further years of post-crisis and pandemic data now warrant a contemporary re-examination.

The theoretically load-bearing feature is how housing enters. In the canonical equation there is *no* classical housing-wealth effect: housing wealth appears only through its interaction with credit conditions, so the housing marginal propensity is zero when credit is fully constrained and is unlocked as credit eases. Illiquid financial and net liquid wealth, by contrast, enter as plain, credit-invariant propensities. This has an immediate empirical consequence. A specification that enters a standalone housing-wealth ratio is *not* the credit-conditioned equation, and reading an insignificant standalone housing coefficient as a failed housing-wealth effect is a category error — the theory predicts that coefficient to be near zero absent the interaction.

**The identification question.** The credit-conditioned equation is, in its original form, a *system*: the credit index is a common factor whose loadings are pinned down across four equations. Practitioners, forecasting units and applied researchers, however, overwhelmingly work with the *single* consumption equation, because the balance-sheet and house-price data needed for a full system are scarce and because a single equation is what fits inside a policy model. The question this paper poses is therefore not "does the credit-conditioned model hold in Australia?" but the sharper and more useful one: **what can a single aggregate consumption equation, estimated off Australian data alone, actually identify about the credit-conditioned mechanism, and what must it leave to the system or to a longer sample?** The answer turns out to draw a clean line — the data identify the *form* and the *adjustment mechanism*, but not the *magnitudes* of the individual structural channels — and that line, rather than any single coefficient, is the paper's contribution.

**Contribution.** We make four contributions, all oriented around that line. First, we establish that the data support a genuine long-run (cointegrating) relationship between consumption and its determinants. A bounds test in the Pesaran, Shin and Smith (2001) tradition clears the upper critical bound for both estimated forms, and the relationship is carried by the joint wealth/housing/credit/permanent-income vector — not by the imposed unit-income consumption-to-income ratio, which we show is itself non-stationary. Second, we isolate what makes the adjustment mechanism come alive. Decomposing the estimated speed of adjustment into a functional-form component and a sample/credit-series component, on a common estimation window, shows that functional form, not the larger sample, is decisive for the speed. Third, we quantify the identifying content of the credit index by carrying it through the full multiplicative construction and benchmarking it against a random-knot placebo, on which it ranks highly. Fourth, and most importantly, we propagate the two first-order sources of estimation uncertainty that a single equation must confront — the selection of the credit-index knots and the construction of the permanent-income series — through a nested bootstrap, and find that only the sign and non-zero magnitude of the error-correction speed survive. Every individual structural elasticity becomes statistically indistinguishable from zero; none can reject the original Australian calibration; and the permanent-income channel reverses sign under a causal, real-time measure. We also contribute a household-sector dataset back-extended to 1976Q3 and use it to test directly whether sample length is the binding constraint (it is not).

The reframe this implies is deliberate. The strongest version of this paper is not "the Australian estimate confirms the original calibration" — every such positive claim carries a confound or an inference gap. It is the methodological statement, near-unrejectable on the evidence assembled here, that *a single credit-conditioned aggregate equation can identify that Australian consumption error-corrects to a credit-sensitive long-run relation, but cannot identify the magnitudes of the individual channels.* The honesty is the contribution.

This matters beyond the Australian case because the single-equation implementation is the one that travels. Central-bank forecasting units, cross-country comparisons and applied studies routinely estimate the consumption block alone, drop the credit interactions into a policy model, and report the structural elasticities as though they were identified. Our results are a caution against that practice stated in its own terms: with a carefully assembled national dataset, a faithful implementation of the form, and every robustness exercise the two-step estimator invites, the individual credit-conditioned elasticities still cannot be separated from zero, and the delta-method intervals that would suggest otherwise are anti-conservative by factors ranging from five to nearly two hundred. The discipline the paper recommends — propagate the construction of the credit index and the permanent-income measure into the reported intervals, and headline only what survives — is portable to every national application in this family. Where it leaves the individual magnitudes unidentified, that is information about the limits of the design, not a defect of the data.

**Roadmap.** Section 2 places the paper in three literatures. Section 3 sets out the equation and argues, from its algebra and the collinearity of its credit channels, what one equation can and cannot separate. Section 4 documents the data, including the back-extension. Section 5 fixes the empirical strategy: three named specifications and the construction and placebo-testing of the credit index. Section 6 reports results — what transfers, what cannot be identified, the long-run verdict, the form-versus-sample decomposition, and the real-time inferential basis. Section 7 draws the implications for policy models. Section 8 concludes.

---

## 2. Literature

The paper sits at the intersection of three literatures: the credit-conditioned "latent interactive" consumption tradition; the Australian empirical consumption literature, historically built on constant-propensity wealth effects; and the smaller body of work disciplining permanent-income measurement.

**Theory and the credit-conditioned form.** The empirical model descends from the Davidson, Hendry, Srba and Yeo (1978) error-correction consumption function and the permanent-income hypothesis of Friedman (1957) and Hall (1978), with Engle and Granger (1987) supplying the cointegration framework and Hendry and Krolzig (2005) and Doornik (2009) the general-to-specific reduction that disciplines the short-run dynamics. A long line of work documents departures from strict permanent-income behaviour — Campbell and Mankiw (1989, 1991) on excess sensitivity to current income, Carroll and Kimball (1996) and Carroll (2001) on precautionary concavity and the buffer-stock distinction between liquid and illiquid wealth (Deaton 1992). Muellbauer (2007) integrated these strands with housing collateral and credit conditions: when credit is tight, housing collateralises borrowing only weakly and a down-payment hurdle dampens consumption; when credit is loose, housing wealth and consumption become tightly linked. The functional form that expresses this is the one estimated below, in which the credit index multiplies several channels jointly and housing enters *only* through its credit interaction. The framework was operationalised across countries — Aron, Duca, Muellbauer, Murata and Murphy (2012) on Japan, the UK and the US; Duca, Muellbauer and Murphy (2010) on the crisis; Duca and Muellbauer (2013) on the flow-of-funds system logic — and, most relevantly here, in single-equation national adaptations by De Bonis, Liberati, Muellbauer and Rondinelli (2020) for Italy and Chauvin and Muellbauer (2018) for France. The Italian study, whose single-equation methodology (net-liquid aggregation, a direct-forecast permanent-income measure, an amortisation-adjusted real rate, and OLS validated against joint estimation) we follow closely, has since been extended by De Bonis, Liberati, Muellbauer and Rondinelli (2023), who argue that net worth is the wrong aggregate for explaining consumption — a direct motivation for the disaggregated wealth treatment used here. Geiger, Muellbauer and Rupprecht (2016) provide the corresponding German application.

**Australian consumption and MARTIN.** Australian consumption modelling outside this tradition has largely used the standalone-wealth-effect form. Tan and Voss (2000) find significant positive housing and financial wealth effects in aggregate time series; Dvornak and Kohler (2003) exploit state-level variation and find larger stock-market than housing propensities. The most direct recent comparator is May, Nodari and Rees (2020), who estimate Australian wealth effects on consumption and whose treatment any single-equation Australian study must engage. None of these interacts housing with credit conditions, so each estimates an unconditional housing propensity that averages over tight- and loose-credit regimes — a property to keep in view when comparing magnitudes. The Reserve Bank's macroeconometric model MARTIN (Cusbert and Kendall 2018; Ballantyne et al. 2019) embeds a consumption block with calibrated wealth elasticities and no explicit credit-index spline; we return to whether a freely estimated equation can discipline it.

**Credit-conditions identification and permanent income.** The credit index is the most contested ingredient. Williams (2009) identifies Australian institutional turning points via unobserved-components analysis (Koopman et al. 2000); the joint system of Muellbauer and Williams (2012) refines this into a spline of smoothed-step dummies identified as a *common factor* across four equations. The institutional chronology is documented in Battellino and McMillan (1989) and Edey and Gray (1996), with Bayoumi (1993) providing cross-country evidence on the consumption response to liberalisation. On permanent income, the standard AR(p)-forecast recipe descends from Hall (1978) and Campbell and Mankiw (1989); the alternative adopted here forecasts the discounted future-income aggregate directly, following De Bonis et al. (2020, Appendix A.2) and related in spirit to Jordà (2005).

**Inference with constructed regressors.** Because a single-equation implementation must first *construct* the credit index and the permanent-income series and then use them as regressors, the estimator is a two-step procedure with generated regressors and a pre-tested spline. The classical warning is Pagan (1984); the two-step variance correction is Murphy and Topel (1985). We take this literature seriously: it is precisely the failure to propagate first-stage uncertainty that, we show below, would overstate what the single equation identifies.

Relative to this literature the paper's position is deliberately modest and methodological. Rather than adding one more national point estimate, it asks what a single equation *can* deliver, and answers by carrying the construction uncertainty the two-step literature warns about all the way into the reported intervals.

Two recent contributions sharpen that position. May, Nodari and Rees (2020) provide the natural Australian benchmark for aggregate wealth effects; our results are consistent with a positive wealth channel of the order they report, but our disaggregated intervals show that a single equation cannot resolve that aggregate into credit-invariant and credit-conditioned components with any precision — which is itself a reason to prefer the disaggregated, credit-conditioned architecture even where its individual coefficients are imprecise. De Bonis, Liberati, Muellbauer and Rondinelli (2023) argue, on Italian data, that net worth is the wrong aggregate for consumption precisely because its components carry different propensities; our finding that the Australian data support the *disaggregated form* while being unable to pin the component magnitudes is the single-equation counterpart of their point. Read together, the two literatures motivate exactly the exercise here: adopt the disaggregated, credit-conditioned form, and be honest about the identification ceiling a single national equation imposes on it.

---

## 3. The equation and what one equation can identify

### 3.1 The canonical consumption equation

In its canonical form the credit-conditioned consumption equation writes the change in log consumption as a speed of adjustment multiplying a long-run bracket, plus short-run dynamics and dummies. The long-run bracket contains six credit-related objects together with the plain wealth propensities and the income restriction:

1. an *autonomous-consumption loading* on the credit index, capturing the direct effect of easier credit on the desired consumption-to-income ratio;
2. a *real-interest-rate* term interacted with credit, so that the sensitivity of consumption to the cost of borrowing is itself credit-state-dependent;
3. a *housing-collateral* term entered **only** as credit times the housing-to-income ratio — there is no free-standing housing-wealth level;
4. an *illiquid-financial* wealth-to-income ratio (equities plus superannuation) entered as a plain, credit-invariant propensity;
5. a *net-liquid* wealth-to-income ratio (liquid assets less total household debt) entered as a plain, credit-invariant propensity;
6. a *permanent-to-current income gap* whose gearing rises linearly with credit;
7. an *affordability* (house-price-to-income) term scaled by one-minus-credit, so that a high price-to-income ratio depresses consumption more when credit is tight; and
8. current income entered with a coefficient restricted to *unity* against lagged consumption, so that the error-correction object is the log consumption-to-income ratio and the equilibrium is a stationary consumption-to-income relation in the flow-of-funds tradition.

Three features are load-bearing. First, the credit index enters the long run *multiplicatively and repeatedly* — through the autonomous loading, the rate interaction, the housing-collateral interaction, the permanent-income gearing and the affordability scaling. It is not an additive shifter but a switch that turns the long-run wealth and income channels on and off as credit conditions ease or tighten. Second, there is no classical housing-wealth effect: the housing propensity is identically zero when credit is fully constrained and is unlocked as credit eases, so it is a collateral / equity-withdrawal channel rather than a pure wealth effect. This is the theoretical core of the paper's *form-is-decisive* result — a specification that enters a standalone housing ratio and reads its coefficient as the housing-wealth effect is mis-testing the theory, since theory predicts that standalone coefficient to be near zero absent the interaction. Third, illiquid-financial and net-liquid wealth enter as plain, credit-invariant propensities, and the unit-income restriction makes the equilibrium a consumption-to-income ratio rather than a freely estimated cointegrating vector.

The permanent-income gearing is itself credit-dependent, rising linearly with the index, and the original Australian application *calibrates* both its intercept and its slope, together with the illiquid-financial propensity and the affordability multiplier, rather than estimating them — the calibrated intercept of 0.20 on the permanent-income gap, a slope of 0.93, an illiquid propensity of 0.022 and an affordability multiplier of 1.2, motivated by a theoretical ceiling on the gearing. The distinction between the *structure* (the interactions and the unit-income restriction) and the *calibration* (the specific imposed values) is central to what follows and is tested directly in Sections 5 and 6.

Because the whole long-run bracket is multiplied by the speed of adjustment, the OLS coefficient on each long-run regressor equals the speed of adjustment times the structural parameter. We therefore recover each structural elasticity as the OLS coefficient divided by the absolute speed of adjustment, and we report both throughout, so that the adjustment-speed channel and the long-run-magnitude channel are separable — a discipline that also makes precise *why* imposing the calibration can collapse the equation. Fixing several structural parameters while iterating to the fixed point implied by the unit-income restriction over-determines the bracket, and the only free margin left, the speed of adjustment, is driven toward zero. Estimation is by OLS with Newey–West standard errors, heteroskedasticity being structural in every full-sample specification; the interacted economic variables are de-meaned over the estimation window, following the original convention, so that the plain propensities are read at sample-mean credit conditions.

### 3.2 Why one equation cannot separate the credit channels

The multiplicative form has a consequence that governs the entire empirical exercise. Each credit-interacted regressor is approximately proportional to the same latent index. On the Australian sample the five credit-related regressors are between roughly 0.66 and 0.97 correlated in absolute value — the autonomous-consumption loading and the permanent-income interaction alone correlate at 0.97, the housing-collateral and affordability terms at 0.91. Regressors this collinear cannot be separately and freely estimated off a single equation: OLS can fit their *sum's* contribution to the residual but cannot allocate it across channels with any precision.

This is not a nuisance to be robustness-checked away; it is the structural reason the original framework identifies the credit channels through cross-equation restrictions rather than within the consumption equation alone. Two things compound it on the Australian sample. The disaggregated balance-sheet data begin only in 1988, after the deregulation episodes that would most sharply distinguish tight- from loose-credit regimes; and, as Section 5.2 documents, the empirically-selected credit index is identically zero before 2007, so the credit interactions are in practice identified off only about seventy quarters. Collinear regressors, a short identifying window, and a latent factor that carries usable variation only in the back third of the sample together set a hard ceiling on what a single equation can separate.

The corollary sets the terms of the whole exercise. Sharpening the individual credit channels requires either the four-equation system or a longer sample that genuinely spans the financial-liberalisation episode; single-equation refinement cannot deliver it. The empirical strategy below is therefore designed not to *maximise* the number of significant coefficients but to establish exactly where that ceiling lies — which objects survive the collinearity, the short window and the construction uncertainty, and which do not. The honest map of that boundary is the deliverable.

---

## 4. Data

The dataset assembles quarterly Australian macroeconomic and household-sector observations from **1976Q3 to 2024Q4**, up to 194 quarters. Real per capita consumption and disposable income are built from the national accounts (household final consumption expenditure and the household income account), deflated by the consumption deflator and divided by the population aged 15 and over, following the original Australian study's population convention. We use gross disposable income as the headline measure, following standard practice (Blinder and Deaton 1985), and report a non-property income alternative in the Online Appendix. Consistent with the unit-income restriction, the disposable-income series is load-bearing for the level of the equilibrium, not merely a scaling variable.

Household balance-sheet stocks — deposits, equities, superannuation, total liabilities, dwellings and net worth — come from the ABS household balance sheet and begin in **1988Q3**. Wealth enters as asset-to-annualised-income ratios: net liquid assets (deposits less total household debt), illiquid financial assets (equities plus superannuation), and housing assets. Following the Italian implementation, net liquid assets nets debt against liquid assets; we test the underlying equal-and-opposite restriction directly and cannot reject it in any specification or sample, though the non-rejection reflects imprecision rather than confirmed exact netting. The real mortgage rate, house-price index, demographic shares and narrative dummies complete the regressor set.

**The back-extension as a contribution.** The 1988Q3 start of the sectoral balance sheet is the deepest single-equation constraint: the financial-liberalisation episode that most cleanly identifies the credit channels — the 1980s deregulation — *predates* the modern disaggregated data. We therefore build a documented back-extension to 1976Q3 from public sources: a long-run house-price compilation, the central bank's M3 and total-credit aggregates, and a historical labour-force series, with disaggregated wealth proxies growth-rate-spliced onto their 1988Q3 official values. Because the liberalisation episode largely predates 1988, this back-extension is the natural route to longer credit variation, and it lets us test directly (Section 6.5) whether sample length is the binding constraint on identifying the credit mechanism.

Permanent income is the discounted weighted average of expected future log income over a forty-quarter horizon. The headline measure is a direct single-regression forecast of the pre-aggregated discounted target on predictors including a labour-force-participation term, deterministic trends and income dynamics, following the Italian methodology. Two properties are disclosed and matter for inference. The measure is *full-sample* — its coefficients use the whole sample, so it embeds information dated after each observation and is a two-sided *measurement* rather than a real-time forecast — and its realised target is computable only through 2014, so the final years are trend-driven out-of-training predictions. We therefore also construct a causal, expanding-window *real-time* variant, and Section 6.6 makes it the inferential basis. Full documentation of the balance-sheet proxies, the splice methodology, the demographic construction and the coverage tiers is provided in an Online Appendix; the discount-and-horizon settings are not load-bearing for the speed of adjustment, and the forecaster *method* rather than the discount calibration is the material choice.

---

## 5. Empirical strategy

### 5.1 Three named specifications

We lead with three specifications and relegate the rest of the ladder to the Online Appendix (Table 1).

**The faithful LIVES specification** enters the equation in its canonical form: housing only through the credit-times-housing interaction, the autonomous-consumption credit loading restored, illiquid financial assets combined into a single term, and the permanent-income gearing freely estimated. It is estimated on 146 quarters (1988Q3–2024Q4), with a pre-pandemic subsample of 126 quarters treated as the identified window, and is the headline throughout.

**The conventional ECM baseline** replaces the credit interactions with plain, constant propensities on each wealth component and admits credit only as a short-run regressor. This is *not* the credit-conditioned equation — it is a generic constant-propensity wealth error-correction model, and prior Australian work (and a naïve reading of the framework) has treated it as though it were. Because its short-run credit term depends on a loan-flow ratio available only from 2002Q3, it binds at 86 quarters. We retain it as the conventional comparator against which the faithful form is the theoretically correct alternative — a matter of theory, not fit — and we do *not* read its insignificant standalone housing coefficient as a failed housing-wealth effect, since the theory predicts that coefficient to be near zero.

**The calibration-imposed variant** takes the faithful form and hard-imposes the original Australian calibration — the permanent-income gearing intercept and slope and the illiquid-financial propensity — rather than estimating them. It isolates whether the Australian *calibrations*, as distinct from the *structure*, transfer.

**Table 1. The three named specifications.** *Source: Authors' calculations.*

| Feature | Faithful LIVES | Conventional ECM baseline | Calibration-imposed variant |
|---|---|---|---|
| Housing enters via | credit × housing only | standalone constant propensity | credit × housing only |
| Credit index | multiplicative, six channels | short-run regressor only | multiplicative, six channels |
| Illiquid financial wealth | combined, estimated | separate, estimated | combined, imposed |
| Permanent-income gearing | freely estimated | freely estimated | imposed (intercept and slope) |
| Estimation window | 1988Q3–2024Q4 (n = 146) | 2002Q3–2024Q4 (n = 86) | 1988Q3–2024Q4 (n = 146) |
| Role in paper | headline | conventional comparator | calibration test |

### 5.2 The credit-conditions index and its placebo battery

The credit index is a spline of smoothed-step dummies at institutional turning points, each knot's sign constrained by institutional history and enforced by drop-on-violation general-to-specific reduction. Rather than impose the original four-knot count on a sample that cannot identify three of those four knots, we begin from a fifteen-knot candidate set spanning the documented Australian financial-policy chronology and let iterated reduction prune knots that are aliased or violate their institutional sign prior.

On the 1988Q3 sample this reduces to **four surviving knots, all post-2007** (the crisis onset, the first-home-buyer episode, the royal-commission lending crackdown, and the pandemic income-support episode). The resulting index has a specific and consequential shape (Figure 1): it is **identically zero from the start of the sample until 2007Q3**, dips slightly negative over the crisis ramp, rises to a normalised peak of one by 2010, plateaus through 2018, then falls steeply to a trough below minus two in late 2020. Every credit channel is therefore identified off roughly **seventy post-2007 quarters**, not the nominal 146 — the pre-crisis half of the sample contributes nothing to the credit coefficients. That all four surviving knots are post-2007 is itself part of the identification story: the post-1988 sample carries usable sign-identifying variation only around the crisis, macroprudential and pandemic episodes.

Whether this spline identifies genuine credit turning points or merely flexibly detrends the consumption residual is testable with random-knot placebos. Two stages matter. At the *additive spline-selection* stage — where the knots are chosen as plain long-run regressors — the deployed construction, run through its own iterated reduction protocol, beats 84 per cent of random draws, moderate support for the knot selection itself. The more demanding test carries the *selected* index through the **full multiplicative construction** — the deployed object that the faithful specification actually uses — and benchmarks it against 198 random-knot draws pushed through the identical construction. Here the institutionally-timed index ranks at the **93rd percentile on model fit** (adjusted R², log-likelihood and information criterion all agree), at the **94th percentile on the joint significance of the credit block** (a Wald statistic of 6.9 against a placebo median of 2.3), and at the **98th percentile on the estimated speed of adjustment**. The institutional knot placement is therefore doing genuine identifying work at the deployed stage, not merely at the selection stage (Table 2).

Two limits on this evidence must be stated at the outset, because they are easy to overread. It is *distributional* evidence that the index *as a whole* is informative — it is not evidence that any *individual* credit-interaction channel is separately identified. And the construction is a two-step procedure with pre-test re-use of the dependent variable, so the faithful specification's fit statistics are conditional on an index pre-fitted to the same series. The placebo quantifies this; the nested bootstrap of Section 6.3 propagates it.

Three further disclosures keep the construction honest. First, a literal replication of the original four-knot spline sits at the placebo *median* on model fit, so the identifying content is a property of the deployed, empirically-selected construction, not of the original knot count imposed on this sample; on the 1988Q3 window only one of the four canonical knots even survives sign-prior reduction, because the deregulation episodes that identify three of them predate the data. Second, the institutional reading of one surviving knot is contestable: the first-home-buyer episode was a fiscal stimulus rather than a lending-standards easing, so its positive sign records a credit-demand event under a credit-supply label. Third, the candidate basis embeds a genuine sign-prior conflict at the early-1990s mortgage-originator entry, which one defensible institutional reading codes as loosening and another as the tail of bank retrenchment; we disclose the conflict rather than silently resolve it. A sectional sign-prior alternative and the original four-knot construction are both retained as robustness benchmarks. None of this changes the central reading: the deployed index carries real but bounded identifying content, and the individual channels it is meant to switch remain unidentified off one equation.

**Table 2. Placebo battery for the credit-conditions index, deployed multiplicative construction (198 random-knot draws).** *Source: Authors' calculations.*

| Metric | Institutional index | Placebo median | Percentile |
|---|---:|---:|---:|
| Model fit (adjusted R²) | 0.824 | 0.761 | 93rd |
| Log-likelihood | 532.2 | 509.7 | 93rd |
| Information criterion (BIC) | −954.8 | −909.7 | 93rd |
| Speed of adjustment (magnitude) | 0.448 | 0.240 | 98th |
| Credit-block joint Wald F | 6.88 | 2.32 | 94th |

---

## 6. Results

### 6.1 What transfers: the adjustment speed and the credit-invariant propensities

Estimated in its faithful form on the full sample, the equation delivers a stable, correctly signed error-correction mechanism. The speed of adjustment is negative and significant, and on the pandemic-controlled samples — which we treat as the identified window, since the full-sample estimate is inflated by the pandemic quarters and fails an upper-bound screen on its magnitude — it clusters tightly at about a quarter of the equilibrium gap closed per quarter (Table 3). The pre-pandemic estimate is −0.266 (t = −4.85), within about 7 per cent of the original Australian FIML estimate of −0.286, and the variant dropping the pandemic quarters and the variant using richer pandemic dummies bracket it at −0.248 and −0.242. The full-sample value of −0.448 (t = −3.57) is reported but not headlined. This is the paper's most robust positive finding: the *mechanism* — that Australian consumption error-corrects to a credit-sensitive long-run relation — transfers, and its speed is close to the original.

The two credit-invariant wealth propensities are right-signed and, at face value, precisely estimated: the net-liquid-asset term is +0.027 (t = 3.75) and the illiquid-financial term +0.015 (t = 3.09) on the full sample, implying structural propensities near 0.060 and 0.035. Both weaken to marginal significance on the pre-pandemic subsample (t of 1.81 and 1.74) and firm up again once the pandemic quarters are handled with richer dummies, a pattern that already hints — ahead of the formal treatment in Section 6.3 — that their apparent precision is not robust. Permanent income enters strongly, +0.459 (t = 4.04) on the full sample and +0.298 (t = 5.81) pre-pandemic.

The permanent-income result carries an open puzzle we disclose rather than re-scale away. Applying the structural-recovery rule, the implied gearing on the permanent-to-current income gap is of order one — about 1.0 on the full sample and 1.1 on the pandemic-controlled variants — which sits *above* the theoretical admissibility ceiling of roughly 0.95 that the original calibration was built to respect. The breach is not an artefact of the crisis-era learning weight applied to the permanent-income series (removing it leaves the gearing near 1.05), and candidate explanations include residual pandemic leverage, the two-sided construction of the measure, and the unit-income restriction forcing the permanent-income gap to absorb low-frequency drift in the consumption-to-income ratio. Section 6.3 shows this gearing is in any case not distinguishable from a wide range of values once construction uncertainty is carried, so the breach is best read as a signal that the *level* of the gearing is imprecisely pinned rather than as a structural estimate to be defended at face value.

The conventional baseline, by contrast, delivers a weaker and sample-fragile equilibrium (−0.239, t = −2.55, on its 86-quarter window) and an insignificant standalone housing coefficient (+0.0022, t = 0.30) — which, under the theory, is expected to be near zero and is *not* evidence of a failed housing-wealth effect. The calibration-imposed variant *collapses* the mechanism: forcing the original permanent-income gearing and illiquid-financial propensity drives the speed of adjustment to −0.030 (t = −0.74), statistically indistinguishable from zero on the full sample, and flips its sign to a significant but wrong-signed +0.041 on the pre-pandemic sample. The mechanism of the collapse is the fixed-point logic of Section 3.1: forcing the gearing to the small calibrated value while the unit-income restriction ties down the rest of the bracket over-determines the long run, and the only free margin left — the speed of adjustment — adjusts toward, and through, zero. The *structure* (the interactions and the unit-income restriction) transfers to Australia; the specific Australian *calibrations* do not, because Australia freely estimates a permanent-income gearing several times the imposed 0.20. This is not, however, licence to claim the data *reject* the calibration: Section 6.3 shows the honest intervals contain the calibrated values. The correct statement is narrower and more robust — imposing the calibration wrecks the fit, but the free estimates are too imprecise to reject it channel by channel. Low power and poor fit under imposition are different facts, and both are true here.

**Table 3. Headline estimates, three specifications (full sample; Newey–West t in parentheses).** *Source: Authors' calculations.*

| Coefficient (OLS) | Faithful LIVES | Conventional ECM baseline | Calibration-imposed variant |
|---|---:|---:|---:|
| Speed of adjustment | −0.448 (−3.57) | −0.239 (−2.55) | −0.030 (−0.74) |
|  — pandemic-controlled | −0.266 (−4.85) | — | +0.041 (2.03) |
| Net liquid assets | +0.027 (3.75) | — | — |
| Illiquid financial assets | +0.015 (3.09) | — | — |
| Standalone housing | — (by construction) | +0.002 (0.30) | — |
| Housing × credit | +0.0025 (0.71) | — | — |
| Permanent-to-current income | +0.459 (4.04) | — | — |
| Observations | 146 | 86 | 146 |
| Adjusted R² | 0.824 | 0.804 | 0.687 |

### 6.2 The long-run verdict

The long-run apparatus rests on establishing that consumption and its determinants actually cointegrate. Two findings must be stated together (Table 4).

First, a bounds test for a level relationship, in the Pesaran, Shin and Smith (2001) tradition, **conclusively supports cointegration for both estimated forms.** The faithful specification returns F = 9.6 and t = −5.1; the conventional baseline returns F = 5.6 and t = −4.9. Both F statistics clear the 5 per cent I(1) upper bound of 3.30, and both t statistics clear the I(1) bound of −4.88. A level (cointegrating) relationship between consumption and its long-run determinants is therefore supported for both forms.

Second — and this is a correction of a natural but mistaken presumption — that relationship is carried by the *full* long-run regressor set acting jointly, **not** by the imposed unit-income consumption-to-income ratio. One might suppose the unit-income restriction delivers a stationary equilibrium error by construction. It does not: ADF and KPSS tests agree unanimously that the consumption-to-income ratio is **non-stationary in every window** — ADF between −2.33 and −2.50 against critical values near −2.88, and KPSS between 0.78 and 1.00 against 0.463. The equilibrium is empirical, established by the bounds test on the joint vector of wealth, housing, credit and permanent income, not definitional.

The calibration-imposed variant is excluded from this verdict. Because its long-run vector is partly hard-calibrated, it enters as a fixed offset rather than a set of freely estimated coefficients, so the bounds procedure is not well-defined for its estimating equation; a diagnostic on its two freely estimated long-run regressors is inconclusive (F = 4.245, between the bounds). Its long run must therefore be described as *imposed and calibrated, not tested.* Long-run and equilibrium language is warranted for the faithful specification and the conventional baseline; it is not available for the calibration-imposed variant.

The methodological significance of this pair of findings is worth drawing out, because it corrects a tempting but wrong shortcut. A single-equation practitioner, faced with an equation whose long-run object is a unit-income consumption-to-income ratio, is inclined to *assert* that the ratio is a stationary equilibrium error by construction — and, if a residual-based cointegration test on the full vector happens to fail, to fall back on that assertion to keep the long-run language. The Australian data refuse both moves. The residual-based route is not the right test for this equation, because it treats the imposed unit-income restriction as a free cointegrating vector; the bounds test, which asks directly whether a level relationship exists among the regressors as they actually enter, is, and it *supports* one. And the fallback assertion is simply false: the consumption-to-income ratio is non-stationary in every window, so the equilibrium cannot be manufactured from the restriction alone. The equilibrium the data support is the *joint* one — wealth, housing, credit and permanent income moving together with consumption — and it is an empirical finding, not a definitional convenience. This is the one place in the paper where the honest answer is unambiguously positive: the long-run relation is real, and it is credit-sensitive.

**Table 4. Long-run verdict.** *Source: Authors' calculations. I(1) 5% bounds: F = 3.30, t = −4.88.*

| | Faithful LIVES | Conventional baseline | Calibration-imposed |
|---|---:|---:|---:|
| Bounds F | 9.57 | 5.59 | infeasible |
| Bounds t | −5.08 | −4.93 | infeasible |
| Verdict | cointegrated | cointegrated | not tested |
| c/y ratio: ADF | −2.50 | −2.43 | — |
| c/y ratio: KPSS | 0.78 | 0.85 | — |
| c/y stationary? | no | no | — |

### 6.3 What cannot be identified: the honest intervals

The precisely-estimated propensities of Section 6.1 are a mirage of the standard errors. A single-equation implementation must *construct* the credit index (by pre-tested knot selection) and the permanent-income series (by full-sample forecast) before using them as regressors; conventional delta-method standard errors hold both constructions fixed, treating pre-tested and generated regressors as though they were data. The classical literature (Pagan 1984; Murphy and Topel 1985) warns that this understates the true sampling variability. To measure it, we run a **nested bootstrap** (199 draws) that re-selects the credit-index knots and re-constructs the permanent-income and error-correction regressors *inside each draw*, and we make these intervals the headline (Table 5).

The result is stark (Figure 2). **Only the speed of adjustment survives.** Its honest 95 per cent interval, [−0.273, −0.077], excludes zero: the sign and non-zero magnitude of the error-correction mechanism are identified. The nested interval sits well inside the delta-method interval, and the nested median magnitude, about 0.17, is attenuated relative to the point estimate — a mechanical consequence of letting the error-correction regressor become a bootstrap-constructed object — so we report the *sign and significance* of the speed as the robust finding and flag the level attenuation.

Every one of the six structural channels is **statistically indistinguishable from zero** once construction uncertainty is honestly carried. The net-liquid and illiquid-financial propensities — apparently significant under the delta method — have nested intervals of [−0.174, 0.317] and [−0.086, 0.164], both spanning zero. The permanent-income gearing spans [−0.124, 1.601]. The two ratio-heavy credit interactions are worse than uninformative: their nested intervals span dozens of units, their point magnitudes meaningless. The delta-method intervals are anti-conservative by a factor of five to six for the wealth and permanent-income channels, and by up to two orders of magnitude for the credit interactions; they belong in an appendix, never as the headline.

Two consequences follow and are binding. **No claim to reject the original Australian calibration is available on any individual channel:** the calibrated value lies *inside* the honest interval for the net-liquid, illiquid-financial, permanent-income and both ratio-heavy credit channels. The single equation is too imprecise to reject the calibration, not confirming of it — low power is not agreement. And the two credit-interaction channels (the affordability term is right-signed but zero-spanning; the housing-collateral term's magnitude is unidentified) cannot support any structural reading. Combined with Section 6.2, the honest picture is that the data identify a *credit-sensitive long-run relation and a well-signed adjustment speed*, and nothing about the *size* of any individual channel.

### 6.4 The credit-interaction channels specifically

It is worth being explicit about which channels fail, because they are precisely the ones the credit-conditioned model treats as distinctive. The housing-collateral, rate and permanent-income-times-credit interactions — the switches that make housing consumability and the cost and forward-lookingness of consumption depend on the credit state — are the channels a reader most wants to see identified, and they are the ones that cannot be. Three facts stack against them. They are identified off only about seventy post-2007 quarters, because the deployed index is identically zero before the crisis (Section 5.2). They are mutually near-collinear, each approximately proportional to the same latent index (Section 3.2). And their point estimates are individually fragile even before the bootstrap: the housing-collateral interaction is right-signed but insignificant in every sample treatment; the permanent-income-times-credit interaction is wrong-signed relative to the calibrated positive slope on the full sample, turning right-signed only on the pre-pandemic window; and the affordability interaction, while right-signed, does not survive the honest intervals. The single equation can tell us that the *level* of the permanent-income gearing is large and that consumption error-corrects to a credit-sensitive relation, but it cannot separate the credit *slope* of any channel from zero. This is the empirical signature of the collinearity that motivates the cross-equation identification in the original framework: the credit interactions carry a joint contribution the equation can detect (the placebo Wald statistic of Section 5.2) but cannot allocate across channels.

**Table 5. Nested-bootstrap 95% intervals (structural elasticities unless noted; 199 draws).** *Source: Authors' calculations.*

| Channel | Point | Nested 95% CI | Excludes 0? | Calibration inside CI? |
|---|---:|---|:--:|:--:|
| Speed of adjustment | −0.448 | [−0.273, −0.077] | **yes** | — |
| Net liquid assets | 0.060 | [−0.174, 0.317] | no | yes |
| Illiquid financial assets | 0.035 | [−0.086, 0.164] | no | yes |
| Permanent income | 1.024 | [−0.124, 1.601] | no | yes |
| Permanent income × credit | −1.14 | [−103, 24] | no | yes |
| Housing × credit | 0.006 | [−3.66, 2.36] | no | yes |
| Affordability × (1 − credit) | 0.062 | [−0.074, 0.376] | no | (just outside) |

### 6.5 Form versus sample: what makes the mechanism come alive

The faithful specification differs from the conventional baseline in two respects at once — functional form *and* a larger sample with a different credit series — so a decomposition is needed to attribute the sharper adjustment speed. We estimate the faithful form on the baseline's *own* 86-quarter window (isolating form) and, separately, the baseline form on the faithful window (isolating sample), and compare to the two baselines (Table 6).

The decomposition is decisive for the speed of adjustment. Isolating functional form on the baseline's own window moves the speed from −0.239 to **−0.542** (t = −3.81) — a larger movement than the entire gap to the full faithful estimate of −0.448 — whereas holding the form fixed and only extending the sample and switching the credit series moves it to just **−0.262** (t = −2.49), an order of magnitude smaller. *The change in adjustment speed is driven by functional form, not by the larger sample.* Much of the apparent weakness of single-equation Australian estimates was a specification artefact.

Two qualifications are mandatory and prevent overreading. For the *significance of the disaggregated wealth channels*, form and sample are complements, not substitutes: on the short window the wealth propensities take the right sign and rough magnitude but do not reach conventional significance (t of 1.45 and 2.09), and only cross the thresholds once the sample is *also* extended. We therefore do *not* claim that form alone activates the wealth channels — only that form alone is decisive for the *speed of adjustment*. And the sample-isolating cell is form-approximate rather than form-identical: the baseline's second-difference credit transform is undefined on the signed spline index, so a level second-difference was substituted; it is not an exact form-swap.

**Table 6. Form-versus-sample decomposition of the adjustment speed.** *Source: Authors' calculations.*

| | Window | Speed of adjustment (t) |
|---|---|---:|
| Conventional baseline (form + short sample) | n = 86 | −0.239 (−2.55) |
| **Faithful form, short window** (isolates form) | n = 86 | **−0.542 (−3.81)** |
| Baseline form, long window (isolates sample) | n = 146 | −0.262 (−2.49) |
| Faithful specification (form + long sample) | n = 146 | −0.448 (−3.57) |

**Is sample length the binding constraint?** The back-extension to 1976Q3 lets us test this directly. Refitting the disaggregated no-credit specification on the extended sample moves its speed of adjustment about 12 per cent toward the original estimate (from −0.182 to −0.203), but the individual wealth coefficients *shrink* rather than strengthen and the net-liquid propensity collapses by roughly 95 per cent. Longer data does not sharpen the structural channels. Consistent with the collinearity argument of Section 3.2 and the placebo evidence of Section 5.2, the binding constraint is the single-equation framing itself, not the post-1988 window.

### 6.6 Real-time permanent income as the inferential basis

A headline structural channel reverses sign under a causal measure, and we make the causal measure the inferential basis rather than a footnote. Under the full-sample permanent-income measure the income-gap coefficient is positive but insignificant (+0.325, t = 1.50); under the **real-time, causally-dated measure it is negative and significant** (−0.145, t = −2.23), with an autoregressive real-time variant agreeing (−0.158, t = −1.68). The sign of the permanent-income channel is therefore *not robust to the information set*, and no positive-sign structural claim can be made on it. The full-sample measure is reported as descriptive only; any forward-looking use of the equation — embedding it in a policy model in particular — must use the real-time variant, under which the speed of adjustment is also materially weaker.

This fragility is one of *measurement*, not endogeneity. Treating permanent income as jointly determined with consumption in a seemingly-unrelated-regressions system barely moves the faithful estimates — the income-gap coefficient shifts by under 2 per cent and the credit interaction and speed of adjustment are essentially unchanged — so simultaneity is not what drives the full-sample result. The information set used to construct the measure is.

### 6.7 Diagnostics and out-of-sample behaviour

The faithful specification fits well (adjusted R² = 0.82, standard error 0.68 per cent of the dependent variable), comfortably better than the conventional baseline (0.80) and the calibration-imposed variant (0.69), with no low-order residual autocorrelation (Breusch–Godfrey AR(1) p = 0.45), though a RESET functional-form test rejects — as it does for nearly every specification in the battery — and a 2008Q3 stability test rejects at the 5 per cent level, consistent with the credit interactions binding only post-crisis (details in the Online Appendix). Heteroskedasticity is structural in every full-sample specification — which is why Newey–West standard errors are used throughout and why the delta-method intervals, even before the nested bootstrap, were never the appropriate inferential object.

Out of sample (Figure 3), against random-walk-with-drift and autoregressive benchmarks over a 36-quarter rolling evaluation, the structural specifications beat the random walk at the one-quarter horizon (root-mean-square error about 0.029 against 0.031) but lose at four and eight quarters, where the drift benchmark is hard to beat. This is an honest limit, reported rather than hidden, and it is consistent with the paper's thesis: a well-identified short-horizon error-correction mechanism sitting on top of long-run channels whose magnitudes the data cannot resolve will forecast well one step ahead and poorly further out. Rolling-window estimation shows the speed of adjustment stable in sign and order of magnitude across the sample, with the expected widening of the credit-channel coefficients over the post-2007 window on which they are identified. Instrumental-variables estimation on current income, a seemingly-unrelated-regressions treatment of permanent income, multi-window structural-break tests, an amortisation-adjusted real rate and a scaled-income alternative are all reported in an Online Appendix; none overturns the central verdict of Sections 6.2–6.4. The form and the mechanism are supported; the individual magnitudes are not identified.

---

## 7. Implications for policy models

The results speak directly to how a freely estimated equation should, and should not, be used to discipline a calibrated policy block such as the Reserve Bank's macroeconometric model.

**What is safe to import is the qualitative structure, not point estimates.** The clean findings — a credit-sensitive long-run relation, a well-signed error-correction speed near a quarter per quarter, disaggregated wealth channels of the right sign, and a housing channel that is regime-dependent rather than a fixed elasticity — are exactly the qualitative architecture a policy model should carry. The central *form-is-decisive* result has a direct corollary for financial-stability analysis: the consumption response to house prices is regime-dependent, larger when credit is loose and muted when tight, and reading an insignificant standalone housing coefficient as "no housing-wealth effect" is a category error. That structure is the policy-relevant object even where the point estimate of the housing channel is imprecise.

**The wealth channel of monetary policy is asymmetric across asset classes.** Liquid and illiquid financial wealth transmit to consumption with well-signed propensities, whereas the housing-collateral channel that the theory makes conditional on credit is, on single-equation post-deregulation Australian data, of the predicted sign but unproven. Movements in mortgage rates that change housing values propagate to consumption at the pandemic-controlled adjustment speed of about a quarter of the gap per quarter, close to the original Australian estimate — implying roughly a quarter of any equilibrium gap closed in the first quarter and about 90 per cent of the adjustment completed within two years. The full-sample speed is inflated by the pandemic and is not the policy-relevant figure.

**Macroprudential and pandemic effects are small and specification-dependent.** The macroprudential rounds of 2014 and 2017 enter as smoothed-step dummies with small coefficients; on the faithful specification only the first round is materially negative and marginally significant, and event-study counterfactuals imply cumulative consumption gaps of only one to two per cent that are not robustly signed across horizons. The honest policy statement is that these dummies detect, at most, a small macroprudential drag, not a large persistent one. The pandemic income-support episode, by contrast, registers as a large and precisely dated level effect, consistent with its scale, but is captured by dummies rather than by the structural channels and so speaks to the size of the shock rather than to any elasticity.

**Permanent-income transmission is the strongest channel but must be used in its real-time form.** Permanent income enters the faithful form strongly and significantly, implying that Australian households respond durably to credible permanent-income shocks — relevant for fiscal-multiplier work. But the sign of the channel is not robust to the information set (Section 6.6), and the gearing exceeds the theoretical admissibility ceiling, so any forward-looking application must use the real-time measure, under which both the sign and the adjustment speed weaken. The Australian permanent-income gearing is a domestic estimate, not a transferred calibration; imposing the small calibrated value collapses the equilibrium.

**What is not safe is any individual magnitude.** Fitting the static long run against the policy model's homogeneous balanced-growth block, the freely estimated net-wealth elasticity is 0.12 against the model's calibrated 0.17 — the same order, but the homogeneity restriction is rejected on Australian data (a Wald statistic of 16.4), and the static levels regression does not itself cointegrate, so the rejection is indicative rather than decisive. More fundamentally, Section 6.3 shows that the disaggregated wealth channels' honest intervals span zero, so the equation cannot adjudicate the model's calibrated elasticity, only corroborate a positive wealth channel of the right order. And the operationally relevant permanent-income measure for any forward-looking use is the real-time variant of Section 6.6, under which both the permanent-income sign and the adjustment speed weaken. The honest position is that this equation is a freely estimated benchmark the calibration is *consistent with*, not a source of point estimates precise enough to replace it. For regime classification specifically — tightening-versus-easing diagnoses — the single-equation credit index should be read as consumption-residual identification, not a structurally identified common credit factor; that requires the joint system.

---

## 8. Conclusion

We set out to determine what a single aggregate consumption equation can identify about the credit-conditioned mechanism in Australia, and the evidence draws a clean line. On the supported side: a bounds test confirms a genuine long-run relationship between consumption and its determinants for both estimated forms, carried by the joint wealth/housing/credit/permanent-income vector and not by the imposed consumption-to-income ratio, which is itself non-stationary; the faithful functional form, not the larger sample, is what sharpens the error-correction speed to within about 7 per cent of the original Australian estimate; and the institutionally-timed credit index carries real identifying content, ranking at the 93rd-to-98th percentile of a random-knot placebo carried through the full multiplicative construction. On the unsupported side: once knot-selection and permanent-income construction uncertainty are honestly propagated through a nested bootstrap, only the sign and non-zero magnitude of the error-correction speed survive; every individual structural wealth, permanent-income and credit-interaction elasticity is statistically indistinguishable from zero; none can reject the original Australian calibration, whose values lie inside the honest intervals; and the permanent-income channel reverses sign under a causal real-time measure.

The thesis is therefore methodological and, on this evidence, near-unrejectable: **a single credit-conditioned aggregate equation can identify that Australian consumption error-corrects to a credit-sensitive long-run relation, but it cannot identify the magnitudes of the individual channels — the data support the form and the adjustment mechanism, not the structural coefficients.** The constructive implication is specific. Sharpening the individual credit channels is not a matter of further single-equation refinement, which the collinearity of the credit interactions forecloses, nor of a longer sample, which the back-extension shows does not strengthen them. It requires the four-equation system that identifies the credit index as a common factor across cross-equation restrictions, or a sample that genuinely spans the financial-liberalisation episode. Where the single equation reaches its ceiling is exactly where the system begins.

Two lines of work follow directly. The first is the four-equation joint estimation, in which the credit index is identified as a common factor across the consumption, house-price, mortgage-stock and equity-withdrawal equations under sign-restricted cross-equation loadings; the collinearity that defeats the single equation is precisely what the common-factor restriction resolves, and the near-zero cross-equation residual correlation we find in a two-equation subset implies the case for the system rests on identification rather than on efficiency. The second is a genuine pre-1988 extension of the disaggregated balance sheet, which would let the credit interactions be identified off the deregulation episodes themselves rather than off seventy post-crisis quarters. Until one of those routes is taken, the honest report on the single equation is the one given here: a supported form, a supported and well-signed adjustment mechanism, a credit index with real aggregate content, and a set of individual structural magnitudes that the data are simply not rich enough to pin down. Reporting that boundary plainly, rather than papering over it with anti-conservative intervals, is what makes the single-equation exercise worth doing.

---

## References

Ando, A. and Modigliani, F. (1963). The "life cycle" hypothesis of saving: aggregate implications and tests. *American Economic Review*, 53(1), 55–84.

Aron, J., Duca, J. V., Muellbauer, J., Murata, K. and Murphy, A. (2012). Credit, housing collateral and consumption: evidence from Japan, the U.K. and the U.S. *Review of Income and Wealth*, 58(3), 397–423.

Backus, D. K. and Purvis, D. D. (1980). An integrated model of household flow-of-funds allocations. *Journal of Money, Credit and Banking*, 12(2), 400–421.

Ballantyne, A., Cusbert, T., Evans, R., Guttmann, R., Hambur, J., Hamilton, A., Kendall, E., McCririck, R., Nodari, G. and Rees, D. (2019). MARTIN has its place: a macroeconometric model of the Australian economy. Reserve Bank of Australia Research Discussion Paper 2019-07.

Battellino, R. and McMillan, N. (1989). Changes in the behaviour of banks and their implications for financial aggregates. Reserve Bank of Australia Research Discussion Paper 8904.

Bayoumi, T. (1993). Financial deregulation and household saving. *Economic Journal*, 103(421), 1432–1443.

Blinder, A. S. and Deaton, A. (1985). The time-series consumption function revisited. *Brookings Papers on Economic Activity*, 1985(2), 465–521.

Campbell, J. Y. and Mankiw, N. G. (1989). Consumption, income, and interest rates: reinterpreting the time series evidence. *NBER Macroeconomics Annual*, 4, 185–216.

Campbell, J. Y. and Mankiw, N. G. (1991). The response of consumption to income: a cross-country investigation. *European Economic Review*, 35(4), 723–756.

Carroll, C. D. (2001). A theory of the consumption function, with and without liquidity constraints. *Journal of Economic Perspectives*, 15(3), 23–45.

Carroll, C. D. and Kimball, M. S. (1996). On the concavity of the consumption function. *Econometrica*, 64(4), 981–992.

Chauvin, V. and Muellbauer, J. (2018). Consumption, household portfolios and the housing market in France. Banque de France Working Paper.

Cusbert, T. and Kendall, E. (2018). Meet MARTIN, the RBA's new macroeconomic model. *RBA Bulletin*, March.

Davidson, J. E. H., Hendry, D. F., Srba, F. and Yeo, S. (1978). Econometric modelling of the aggregate time-series relationship between consumers' expenditure and income in the United Kingdom. *Economic Journal*, 88(352), 661–692.

De Bonis, R., Liberati, D., Muellbauer, J. and Rondinelli, C. (2020). Consumption and wealth: new evidence from Italy. Banca d'Italia Temi di Discussione 1304.

De Bonis, R., Liberati, D., Muellbauer, J. and Rondinelli, C. (2023). Why net worth is the wrong concept for explaining consumption: evidence from Italy. University of Oxford Economics Series Working Paper 1026.

Deaton, A. (1992). *Understanding Consumption*. Oxford: Clarendon Press.

Doornik, J. A. (2009). Autometrics. In J. L. Castle and N. Shephard (eds.), *The Methodology and Practice of Econometrics: A Festschrift in Honour of David F. Hendry*. Oxford: Oxford University Press, 88–121.

Duca, J. V. and Muellbauer, J. (2013). Tobin LIVES: integrating evolving credit market architecture into flow-of-funds based macro models. European Central Bank Working Paper 1581.

Duca, J. V., Muellbauer, J. and Murphy, A. (2010). Housing markets and the financial crisis of 2007–2009: lessons for the future. *Journal of Financial Stability*, 6(4), 203–217.

Dvornak, N. and Kohler, M. (2003). Housing wealth, stock market wealth and consumption: a panel analysis for Australia. Reserve Bank of Australia Research Discussion Paper 2003-07.

Edey, M. and Gray, B. (1996). The evolving structure of the Australian financial system. In *The Future of the Financial System*, RBA Conference Volume, 6–44.

Engle, R. F. and Granger, C. W. J. (1987). Co-integration and error correction: representation, estimation, and testing. *Econometrica*, 55(2), 251–276.

Friedman, M. (1957). *A Theory of the Consumption Function*. Princeton: Princeton University Press.

Geiger, F., Muellbauer, J. and Rupprecht, M. (2016). The housing market, household portfolios and the German consumer. European Central Bank Working Paper 1904.

Hall, R. E. (1978). Stochastic implications of the life-cycle permanent-income hypothesis: theory and evidence. *Journal of Political Economy*, 86(6), 971–987.

Hendry, D. F. and Krolzig, H.-M. (2005). The properties of automatic GETS modelling. *Economic Journal*, 115(502), C32–C61.

Jordà, Ò. (2005). Estimation and inference of impulse responses by local projections. *American Economic Review*, 95(1), 161–182.

Koopman, S. J., Harvey, A. C., Doornik, J. A. and Shephard, N. (2000). *STAMP 6: Structural Time Series Analyser, Modeller and Predictor*. London: Timberlake Consultants.

May, D., Nodari, G. and Rees, D. (2020). Wealth and consumption in Australia. *Australian Economic Review*, 53(1), 105–117.

Modigliani, F. (1963). The life-cycle hypothesis of saving, the demand for wealth and the supply of capital. *Social Research*, 33(2), 160–217.

Muellbauer, J. (2007). Housing, credit and consumer expenditure. In *Housing, Housing Finance, and Monetary Policy*, Federal Reserve Bank of Kansas City Jackson Hole Symposium Proceedings, 267–334.

Muellbauer, J. and Williams, D. (2012). Credit conditions and the real economy: the elephant in the room. CEPR Discussion Paper 8386.

Murphy, K. M. and Topel, R. H. (1985). Estimation and inference in two-step econometric models. *Journal of Business & Economic Statistics*, 3(4), 370–379.

Pagan, A. (1984). Econometric issues in the analysis of regressions with generated regressors. *International Economic Review*, 25(1), 221–247.

Pesaran, M. H., Shin, Y. and Smith, R. J. (2001). Bounds testing approaches to the analysis of level relationships. *Journal of Applied Econometrics*, 16(3), 289–326.

Tan, A. and Voss, G. (2000). Consumption and wealth. Reserve Bank of Australia Research Discussion Paper 2000-09.

Tobin, J. and Dolde, W. (1971). Wealth, liquidity and consumption. In *Consumer Spending and Monetary Policy: The Linkages*, Federal Reserve Bank of Boston Conference Series 5.

Williams, D. M. (2009). House prices and financial liberalisation in Australia. University of Oxford Economics Series Working Paper 432.

Williams, D. M. (2010). Consumption, wealth and credit liberalisation in Australia. University of Oxford Economics Series Working Paper 492.

---

## Figures

**Figure 1. The deployed credit-conditions index, 1976Q3–2024Q4.** The index is identically zero until 2007Q3, rises to a normalised peak of one by 2010 and plateaus through 2018, then falls steeply to a trough below −2 in late 2020; all four surviving knots are post-2007, so the credit channels are identified off roughly seventy quarters. *Source: Authors' calculations.*

**Figure 2. Nested-bootstrap sampling distributions.** The speed of adjustment (left) concentrates away from zero; the structural wealth, permanent-income and credit-interaction elasticities (right) each straddle zero once knot-selection and permanent-income-construction uncertainty are propagated. *Source: Authors' calculations.*

**Figure 3. Out-of-sample forecast performance.** Root-mean-square error of the faithful specification against random-walk-with-drift and autoregressive benchmarks at horizons of one, four and eight quarters; the structural specification wins at one quarter and loses at four and eight. *Source: Authors' calculations.*
