<script>
    import { Alert,
             Button,
             Col,
             Collapse,
             Card,
             CardBody,
             CardFooter,
             CardHeader,
             CardSubtitle,
             CardText,
             CardTitle,
             Input,
             ListGroup,
             ListGroupItem,
             Form,
             FormGroup,
             Row} from "sveltestrap";

    import FeatureSpecUserType from './FeatureSpecUserType.svelte';

    export let spec = {};
    let isOpen = false;
    let saveAlertVisibile = false;
    let failAlertVisibile = false;
    let failAlertMessage = "";

    function toggle() {
        isOpen = !isOpen;
    };

    async function save() {
        // Reset notifications
        saveAlertVisibile = false;
        failAlertVisibile = false;

        let response = await fetch('/v0/featureSpecs/', {
            method: 'POST',
            headers: {
                'content-type': 'application/json'
            },
            body: JSON.stringify(spec)
        });
        let fulfilledResponse = await response;
        if ( fulfilledResponse.ok) {
            saveAlertVisibile = true;
        } else {
            let responseObj = await fulfilledResponse.json()
            console.log(responseObj);
            failAlertVisibile = true;
            failAlertMessage = responseObj['error']['what'];
        };
    }

</script>

<main>
    <Col xs="4" align="center">
    <Card>
        <CardHeader on:click={toggle} >
            <CardTitle>{spec.name}</CardTitle>
        </CardHeader>
        <Collapse {isOpen}>
        <CardBody>
            <form on:submit|preventDefault={save}><FormGroup>

            <Row>
                <Alert color="success" isOpen={saveAlertVisibile} toggle={() => (saveAlertVisibile = false)}>
                    Feature Flag Saved!
                </Alert>
                <Alert color="danger" isOpen={failAlertVisibile} toggle={() => (failAlertVisibile = false)}>
                    Feature Flag NOT Saved!
                    {failAlertMessage}
                </Alert>

            </Row>

            <Row>
                <Col>Boolean Enabled</Col>
                <Col><Input type="checkbox" bind:checked={spec.boolean} /></Col>
                <Col>{spec.boolean}</Col>
            </Row>

            {#if spec.rollout_start } <Row>
            Rollout Start:<Input
                type="datetime"
                name="rollout_start"
                id="exampleDatetime"
                placeholder="{spec.rollout_start}"
                bind:value="{spec.rollout_start}" />
            </Row> {/if}

            {#if spec.rollout_start }<Row>
            Rollout End: <Input
                type="datetime"
                name="rollout_end"
                id="exampleDatetime"
                placeholder="{spec.rollout_end}"
                bind:value="{spec.rollout_end}" />
            </Row>{/if}

            {#each spec.user as userSpec }
            <Row>
                <FeatureSpecUserType userSpec={userSpec} />
            </Row>
            {/each}

            <Row>
                <Button type="submit" >Save</Button>
            </Row>

        </FormGroup></form>
        </CardBody>
        </Collapse>

    </Card>
    </Col>
</main>

<style>
</style>
