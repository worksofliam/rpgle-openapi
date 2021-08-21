
//Used to create data-structures from openapi schemas

module.exports = class Structs {
  constructor() {
    this.lines = [];
  };

  generateStruct(object, structName) {
    const types = {
      'number': `Packed(30:15)`,
      'boolean': 'Ind',
      'integer': 'Int(20)'
    };
  
    var currentStruct = [];
  
    currentStruct.push(`Dcl-Ds ${structName}_t Qualified Template;`);
  
    var currentProperty;

    switch (object.type) {
      case 'object':
        for (const name in object.properties) {
          currentProperty = object.properties[name];
      
          if (currentProperty.description) currentStruct.push(`  //@ ${currentProperty.description}`);
          switch (currentProperty.type) {
            case 'string':
              currentStruct.push(`  ${name} Varchar(${currentProperty.maxLength || 64})${currentProperty.default ? ` Inz('${currentProperty.default}')` : ``};`);
              break;

            case 'boolean':
              currentStruct.push(`  ${name} ${types[currentProperty.type]}${currentProperty.default !== undefined ? ` Inz(${currentProperty.default ? `*on` : `*off`})` : ``};`);
              break;

            case 'number':
            case 'integer':
              currentStruct.push(`  ${name} ${types[currentProperty.type]}${currentProperty.default ? ` Inz(${currentProperty.default})` : ``};`);
              break;
      
            case 'object':
              this.generateStruct(currentProperty, `${structName}_${name}`);
              currentStruct.push(`  ${name} LikeDS(${structName}_${name}_t);`);
              break;
      
            case 'array':
              currentStruct.push(`  ${name}_len Uns(5);`);
              if (currentProperty.items.type === "object") {
                this.generateStruct(currentProperty.items, `${structName}_${name}`);
                currentStruct.push(`  ${name} LikeDS(${structName}_${name}_t) Dim(100);`);
              } else {
                currentStruct.push(`  ${name} ${types[currentProperty.items.type]} Dim(100);`);
              }
              break;

            default:
              currentStruct.push(`  //@ ${name} not supported. Maybe oneOf or anyOf.`);
              break;
          }
        }
        break;
    
      case 'array': //When the base type is an array
        currentStruct.push(`  items_len Uns(5);`);
        currentProperty = object;

        if (currentProperty.items.type === "object") {
          this.generateStruct(currentProperty.items, `${structName}_items`);
          currentStruct.push(`  items LikeDS(${structName}_items_t) Dim(100);`);
        } else {
          currentStruct.push(`  items ${types[currentProperty.items.type]} Dim(100);`);
        }
        break;
    }

    currentStruct.push(`End-Ds;`, '');
  
    this.lines.push(...currentStruct);
  }
}
