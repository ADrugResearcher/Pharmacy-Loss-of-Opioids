
Link to visualization: https://tinyurl.com/missing-pharma

If you find this or any of my work valuable, please consider making a donation to [DULF's Legal Fund](https://dulf.ca/dulf-aid/) or [the Harm Reduction Advocacy Collective](https://www.gofundme.com/f/help-support-harm-reduction-advocacy-collective-hrac) and let me know!


# Missing Pharma Opioids
An examination of the data of missing pharma opioids in BC, AB & Ontario.

The data and visualization are based on two data sources. The first Tara Carman's reporting on drug losses: https://github.com/taracarman/druglosses.

In 2025 I requested the same data from Health Canada, and ran into a bit of a wall. They were concerned that with the pre-existing data, the new data and news articles on the topic one would be able to deanonymize the cases where facilities had reported thefts of drugs. As such, the 2025 data is missing the subtype for thefts. As well, in Carman's original reporting, the author requested several years of data. Because my request was for only one year, Health Canada was worried that providing information on the Yukon, Nunavut and the North West Territories might potentially deanonymize pharmacies in the North. As such, the data provided here groups together the three Territories.

As well, I have cleaned up a handful of typo's, spelling mistakes and a few different spellings of the same drug (e.g., diamorphine vs diacetylmorphine). Although the data provided by Health Canada included several pharmaceuticals and precursor chemicals I have for the most part excluded those in favour of 4 classes of drugs, "Sedatives/ Hypnotics", "Stimulants", "Opioids" & "Dissociatives".


**Notes about the data/calculating from Incidents**
When one browses the data they'll notice fairly quickly that there is a diversity of reports in here. Pharmacies lose and destroy drugs for entirely legitimate reasons all the time. Depending on the facility and their reporting practices one will find reports included here of half a tablet being reported to the government as loss unexplained. Because the data does not allow us to ID the same facility over time right beside 100 tablets reported as 'loss unexplained.' Both of these incidents could have nothing to do with diversion. It is impossible to claim that all of the reports are indicative of diversion. Small-time reports are the rule, not the exception in this data. 

The date can also be misleading. Some of these reports of losses are from facilities reporting multiple events spanning several years at the same time. As Health Canada made it easier to report lost drugs around 2017, an understandable backlog of reports likely occurred at that time. Also incidents may be reported several years later. The more recent ATIP uncovered several events that occurred prior to 2024 but were only reported in 2025.

For losses, I have automatically removed incidents where fewer than 100 units have been reported in the visualization, although the user can include them in the visualization if they want. While some subset of those reports are from diversion how many of them are is impossible to know. By removing the bulk of small cases we can begin to trace a pattern of shifts and compare different provinces. Using population as a rough means of eliminating differences between provinces due to the number of residents (e.g., ON has more people and will therefore have more pharmacies than BC, where a greater number of events can therefore occur) we can look at general trends of the opioids that go missing from the supply chain.

**Variables in the original dataset (renamed by me for ease):**

ID: Basically the incident report, so if a pharmacy was robbed and lost 10 hydromorphone pills, they probably also had 10 oxy's stolen etc.

province: self-explanatory

date_loss = date items lost

report_date = when was it filed

Brand name: Drug Brand name, sometimes includes the unit

Generic: what the generic name of the substance is

Quant: How many units were taken

Unit: what is it measured in, e.g., pouches, tablets, capsules, ML

Substance_data: Mg/unit

subs_quant: quant x substance_data

facility: location

incident_type: Loss/theft

incident_subtype: breakdown of the type of event

incident_other: in a few cases there is greater explanation of what occurred.
