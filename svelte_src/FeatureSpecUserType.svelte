<script>
    import {
             Button,
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

    import CloseIcon from './CloseIcon.svelte';
    export let userSpec = [];

    let castTypes = [
      "string",
      "integer"
    ];

    let castType;
    let isMultiValue;

    let typeCastFunMap = {};
    typeCastFunMap["string"] = String;
    typeCastFunMap["number"] = Number;
    typeCastFunMap["integer"] = Number;

    if (typeof(userSpec.value) == "object") {
      isMultiValue = true;
      if ( userSpec.length > 0 ) {
        castType = typeof(userSpec.value[0]);
      } else {
        castType = "string";
      }
    } else {
      isMultiValue = false;
      castType = typeof(userSpec.value)
    };

    let isOpen = false;



    function castValue() {
      let typeCastFun = typeCastFunMap[castType];
      if ( isMultiValue == true ) {
        userSpec.value = userSpec.value.map(typeCastFun);
      } else  {
        userSpec.value = typeCastFun(userSpec.value);
      }
    };

    function changeComparator() {
     if (userSpec.comparator == "=" ) {
        isMultiValue = false;
        userSpec.value = userSpec.value[0];
     } else if (userSpec.comparator == "in") {
        isMultiValue = true;
        userSpec.value = [userSpec.value];
     } else {
        alert("Unknown comparator type")
     }
    };
    function addNewArrayValue() {
      let typeCastFun = typeCastFunMap[castType];
      userSpec.value = [...userSpec.value, typeCastFun("")]
    };
    function removeUserSpecArrayValue(arrayValue) {
        return function () {
            userSpec.value = userSpec.value.filter(i => i !== arrayValue);
        };
    };

</script>

      <Row no-gutter>
        <Col xs=6><Input title="User property" bind:value={userSpec.property} /></Col>
      </Row>
      <Row>
        <Col xs=3 >
          <select title="Type of value" bind:value={castType} on:change="{castValue}">
            {#each castTypes as type }
              <option>{type}</option>
            {/each}
          </select>
        </Col>
        <Col xs=2 >
          <select title="How to compare" bind:value="{userSpec.comparator}" on:change="{changeComparator}" >
            <option>=</option>
            <option>in</option>
          </select>
        </Col>
        <Col></Col>
      </Row>

      {#if isMultiValue }
        {#each userSpec.value as valueItem}
        <Row>
          <Col xs=8>
            <Input title="User value" bind:value={valueItem} on:change="{castValue}" />
          </Col>
          <Col xs=2><Button color=dark outline on:click="{removeUserSpecArrayValue(valueItem)}"><CloseIcon /></Button></Col>
        </Row>
        {/each}
        <Row>
          <Col xs=2 />
          <Col xs=4><Button on:click="{addNewArrayValue}">Add item</Button></Col>
          <Col xs=2 />
        </Row>

      {:else}
      <Row>
        <Col xs=8>
          <Input bind:value={userSpec.value} on:change="{castValue}" />
        </Col>
        <Col />
      </Row>
      {/if}


<style>
</style>
