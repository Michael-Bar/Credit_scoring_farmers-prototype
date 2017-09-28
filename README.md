# One Acre Fund predictive model 

Prototype model utilizing CSV downloads to generate predicted probabilities that *groups* of farmers will default on the repayment deadline (note that v2.0 is currently being built into OAF SQL servers). 

## Overview

A group-level default event is defined as any single client within a lending circle defaulting on their loan at the repayment deadline (OAF provides loans without repayment plans, so "days-past-due" etc. are not applicable). If a single member of a group defaults, then her entire group (6-12 members) is black-listed for subsequent seasons, therefore the aim of this model is to identify groups at risk of default, and flag them for action from OAF loan officers and/or call centres.

This model is currently in action across Kenya, Rwanda and Tanzania, representing >85% of our clients.
