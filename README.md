# nf-lv-viz
A prototype app for visualizing latent variable analyses of NF data. 


To deply to ShinyApps.io:

- Enable workflows in the GitHub repository
- Under [secrets](https://github.com/Sage-Bionetworks/nf-lv-viz/settings/secrets/actions) click 'New repository secret'
- Enter secrets for `RSCONNECT_USER`, `RSCONNECT_TOKEN`, and `RSCONNECT_SECRET`, the values for which are saved in Sage's LastPass.
- Optional: If you experience breaking builds due to excessive anonymous GitHub API requests, you can also enter a GitHub PAT as `PAT_FOR_SHINYAPPS_ACTION`.
- Trigger the GitHub action.
- Check out the app here: https://sagebio.shinyapps.io/nf-lv-viz-staging.
- After verifying correctness, create a Git branch named release*, e.g., `release-1.0`.
- The app' will become available at https://sagebio.shinyapps.io/nf-lv-viz
