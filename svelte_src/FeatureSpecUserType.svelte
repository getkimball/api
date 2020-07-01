<script>
    import {

             Col,
             Collapse,
             Dropdown,
             DropdownItem,
             DropdownMenu,
             DropdownToggle,
             Input,
             FormGroup,
             ListGroup,
             ListGroupItem,
             Row} from "sveltestrap";

    export let userSpec = [];

    let comparatorTypes = [
      "string",
      "integer"
    ];

    let comparatorType = typeof(userSpec.value);
    if ( comparatorType == "number" ) {
        comparatorType = "integer";
    }

    let isOpen = false;

    function castValue() {
      if ( comparatorType == "string") {
          userSpec.value = String(userSpec.value);
      } else if ( comparatorType == "integer") {
          userSpec.value = Number(userSpec.value);
      } else {
          alert("Type not recognized for casting");
      };
    }

</script>


<main>

  <ListGroup>
  <ListGroupItem>
      <Input bind:value={userSpec.property} />

      <FormGroup>
        <Input type=select bind:value="{userSpec.comparator}">
          <option>=</option>
          <option>in</option>
        </Input>
      </FormGroup>

      <FormGroup>
        <select bind:value={comparatorType} on:change="{castValue}">
          {#each comparatorTypes as type }
            <option>{type}</option>
          {/each}
        </select>
      </FormGroup>

      <Input bind:value={userSpec.value} on:change="{castValue}" />
  </ListGroupItem>
  </ListGroup>

</main>

<style>
</style>
