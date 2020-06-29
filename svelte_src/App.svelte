<script>
    import { onMount } from "svelte";
    import { Col,
             Container,
             FormGroup,
             Input,
             Row } from "sveltestrap";

    import FeatureSpec from './FeatureSpec.svelte';

    let featureSpecs = [];
    onMount(async () => {
        const res = await fetch(`v0/featureSpecs`);
        const json = await res.json();
        featureSpecs = json.featureSpecs;
    });

    let newFlagName = "";
    async function createNewFlag() {
        let newFlag = {
            "name": newFlagName,
            "boolean": false,
            "user": [],

        };
        let response = await fetch('/v0/featureSpecs/', {
            method: 'POST',
            headers: {
                'content-type': 'application/json'
            },
            body: JSON.stringify(newFlag)
        });
        let fulfilledResponse = await response;
        if ( fulfilledResponse.ok) {
            // saveAlertVisibile = true;
            featureSpecs = [...featureSpecs, newFlag];

        } else {
            let responseObj = await fulfilledResponse.json()
            console.log(responseObj);
            // failAlertVisibile = true;
            // failAlertMessage = responseObj['error']['what'];
        };

    }

</script>

<main>
    <Row>
        <Col>
        <h1>Kimball Features</h1>
        </Col>
    </Row>

    <Row>
    <form on:submit|preventDefault={createNewFlag}><FormGroup>
        <Col><Input bind:value={newFlagName} /></Col>
        <Col><Input type=submit value="Create Flag" /></Col>

    </FormGroup></form>
    </Row>

    {#each featureSpecs as featureSpec }
        <FeatureSpec spec={featureSpec} />
    {/each}

</main>

<style>
    main {
        text-align: center;
        padding: 1em;
        max-width: 240px;
        margin: 0 auto;
    }
    @font-face{
        font-family: 'Kimball';
        src: url('/fonts/limerick_serial-light-webfont.woff') format('woff');
    }

    h1 {
        color: #ff3e00;
        text-transform: uppercase;
        font-family: Kimball;
    }

    @media (min-width: 640px) {
        main {
            max-width: none;
        }
    }
</style>
