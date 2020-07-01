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
    };

    function ISODateString(d){
        function pad(n){return n<10 ? '0'+n : n}
        return d.getUTCFullYear()+'-'
            + pad(d.getUTCMonth()+1)+'-'
            + pad(d.getUTCDate())+'T'
            + pad(d.getUTCHours())+':'
            + pad(d.getUTCMinutes())+':'
            + pad(d.getUTCSeconds())+'Z'
    };

    function addRolloutSpec() {
        let now = new Date();
        let defaultEnd = new Date(now.valueOf());
        defaultEnd.setDate(defaultEnd.getDate() + 2);

        spec.rollout_start = ISODateString(now);
        spec.rollout_end = ISODateString(defaultEnd);
    };

    function removeRolloutSpec() {
        delete spec.rollout_end;
        delete spec.rollout_start;
        spec = spec;
    };

    function addNewUserSpec() {
        let newUserSpec = {
            "property": "user property",
            "comparator": "=",
            "value": "user value"
        };
        spec.user = [...spec.user, newUserSpec];
    };

    function removeUserSpecCallback(userSpec) {
        return function () {
            spec.user = spec.user.filter(i => i !== userSpec);
        };
    };

</script>

<main>
    <Col xs="4" align="center">
    <Card>
        <CardHeader on:click={toggle} >
            <CardTitle>{spec.name}</CardTitle>
        </CardHeader>
        <Collapse {isOpen}>
        <CardBody>

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
                <Col>Always True</Col>
                <Col><Input type="checkbox" bind:checked={spec.boolean} /></Col>
                <Col>{spec.boolean}</Col>
            </Row>

            {#if spec.rollout_start }
            <Row>
            Rollout Start:<Input
                type="datetime"
                name="rollout_start"
                id="exampleDatetime"
                placeholder="{spec.rollout_start}"
                bind:value="{spec.rollout_start}" />
            </Row>
            <Row>
            Rollout End: <Input
                type="datetime"
                name="rollout_end"
                id="exampleDatetime"
                placeholder="{spec.rollout_end}"
                bind:value="{spec.rollout_end}" />
            </Row>
            <Row>
                <Button on:click="{removeRolloutSpec}">Remove rollout spec</Button>
            </Row>
            {/if}

            {#each spec.user as userSpec }
            <Row>
                <FeatureSpecUserType userSpec={userSpec} />
                <Button on:click="{removeUserSpecCallback(userSpec)}">Remove user spec</Button>
            </Row>
            {/each}

            <Row>
                {#if !spec.rollout_start }
                <Button on:click="{addRolloutSpec}">Add rollout spec</Button>
                {/if}
                <Button on:click="{addNewUserSpec}">Add user spec</Button>
                <Button type="submit" on:click={save} >Save</Button>
            </Row>

        </CardBody>
        </Collapse>

    </Card>
    </Col>
</main>

<style>
</style>
