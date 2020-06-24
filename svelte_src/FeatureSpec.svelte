<script>
    import { Col, Row } from "sveltestrap";

    export let spec = {};
</script>

<main>
    <Row>
        <Col xs=4>
        {spec.name}
        </Col>
        <Col xs=1>
        {spec.boolean}
        </Col>
    </Row>
</main>

<style>
</style>
