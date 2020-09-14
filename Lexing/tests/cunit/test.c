#include <stddef.h>
#include "../../string-helpers.h"
#include <CUnit/Basic.h>
#include <CUnit/TestDB.h>
#include "test.h"

/* The suite initialization function.
 * Opens the temporary file used by the tests.
 * Returns zero on success, non-zero otherwise.
 */
int init_suite1(void) {
  return 0;
}

/* The suite cleanup function.
 * Closes the temporary file used by the tests.
 * Returns zero on success, non-zero otherwise.
 */
int clean_suite1(void) {
  return 0;
}

/* The test initialization function.
 * Opens the temporary file used by the test.
 */
void init_test1(void) {
  return;
}

/* The test cleanup function.
 * Closes the temporary file used by the test in particular.
 */
void clean_test1(void) {
  return;
}

/* Simple test of is_digit().
 */
void testISDIGIT_Integers(void) {
  CU_ASSERT_FALSE(is_digit(0));
  CU_ASSERT_FALSE(is_digit(9));
  CU_ASSERT_FALSE(is_digit(-1));
}

void testISDIGIT_Digits(void) {
  CU_ASSERT_TRUE(is_digit('1'));
  CU_ASSERT_TRUE(is_digit('0'));
  CU_ASSERT_TRUE(is_digit('9'));
}

void testISDIGIT_EscChars(void) {
  CU_ASSERT_FALSE(is_digit('\0'));
  CU_ASSERT_FALSE(is_digit('\"'));
  CU_ASSERT_FALSE(is_digit('\n'));
}

/* Simple test of is_alpha().
 */
void testISALPHA_Others(void) {
  CU_ASSERT_FALSE(is_alpha('1'));
  CU_ASSERT_FALSE(is_alpha(' '));
}

void testISALPHA_Char(void) {
  CU_ASSERT_TRUE(is_alpha('a'));
  CU_ASSERT_TRUE(is_alpha('Z'));
  CU_ASSERT_TRUE(is_alpha('C'));
}

void testISALPHA_EscChars(void) {
  CU_ASSERT_FALSE(is_alpha('\a'));
  CU_ASSERT_FALSE(is_alpha('\"'));
  CU_ASSERT_FALSE(is_alpha('\n'));
}

/* Simple test of is_space().
 */
void testISSPACE_Other(void) {
  CU_ASSERT_FALSE(is_space('d'));
}

void testISSPACE_Space(void) {
  CU_ASSERT_TRUE(is_space(' '));
  CU_ASSERT_TRUE(is_space('\n'));
}

void testISSPACE_Esc(void) {
  CU_ASSERT_FALSE(is_space('\"'));
}

/* Simple test of is_identifier_component().
 */
void testISIDENTIFIER_Integers(void) {
  CU_ASSERT_FALSE(is_identifier_component(' '));
  CU_ASSERT_FALSE(is_identifier_component('('));
  CU_ASSERT_FALSE(is_identifier_component('*'));
  CU_ASSERT_FALSE(is_identifier_component('-'));
}

void testISIDENTIFIER_identifier(void) {
  CU_ASSERT_TRUE(is_identifier_component('1'));
  CU_ASSERT_TRUE(is_identifier_component('d'));
  CU_ASSERT_TRUE(is_identifier_component('_'));
}

void testISIDENTIFIER_EscChars(void) {
  CU_ASSERT_FALSE(is_identifier_component('\0'));
  CU_ASSERT_FALSE(is_identifier_component('\"'));
  CU_ASSERT_FALSE(is_identifier_component('\n'));
}

/* Simple test of is_valid_identifier().
 */
void testISVALID_Invalid(void) {
  CU_ASSERT_FALSE(is_valid_identifier("3)a"));
  CU_ASSERT_FALSE(is_valid_identifier(" "));
}

void testISVALID_Valid(void) {
  CU_ASSERT_TRUE(is_valid_identifier("b3_2"));
  CU_ASSERT_TRUE(is_valid_identifier("adfd"));
}

void testISVALID_Esc(void) {
  CU_ASSERT_FALSE(is_valid_identifier("\4n"));
  CU_ASSERT_FALSE(is_valid_identifier("\""));
  CU_ASSERT_FALSE(is_valid_identifier("\n"));
}

/* Simple test of str_concat().
 */
void testSTRCONCAT_First(void) {
  char *str1[] = {"This ", "is ", "the ", "first ", "string."};
  char *concat1 = str_concat(str1, 5);
  CU_ASSERT_EQUAL(25, strlen(concat1));
  CU_ASSERT_NSTRING_EQUAL("This is the first string.", concat1, 2);
}

void testSTRCONCAT_Second(void) {
  char *str2[] = {"And ", "the ", "second ", "HERE!"};
  char *concat2 = str_concat(str2, 4);
  CU_ASSERT_EQUAL(20, strlen(concat2));
  CU_ASSERT_NSTRING_EQUAL("And the second HERE!", concat2, 2);
}

void testSTRCONCAT_Third(void) {
  char *str3[] = {"Finally", ", ", "the ", "last ", "one."};
  char *concat3 = str_concat(str3, 5);
  CU_ASSERT_EQUAL(22, strlen(concat3));
  CU_ASSERT_NSTRING_EQUAL("Finally, the last one.", concat3, 2);
}

/* The main() function for setting up and running the tests.
 * Returns a CUE_SUCCESS on successful running, another
 * CUnit error code on failure.
 */
int main() {
  CU_TestInfo isdigit_tests[] = {{"Test actual digits", testISDIGIT_Digits},
                                 {"Test esc chars", testISDIGIT_EscChars},
                                 {"Test numbers", testISDIGIT_Integers},
                                 CU_TEST_INFO_NULL};

  CU_TestInfo isalpha_tests[] = {{"Test actual alphabets", testISALPHA_Others},
                                 {"Test characters", testISALPHA_Char},
                                 {"Test esc chars", testISALPHA_EscChars},
                                 CU_TEST_INFO_NULL};

  CU_TestInfo isspace_tests[] = {{"Test actual space", testISSPACE_Space},
                                 {"Test esc chars", testISSPACE_Esc},
                                 {"Test others", testISSPACE_Other},
                                 CU_TEST_INFO_NULL};

  CU_TestInfo isidentifier_tests[] = {{"Test actual identifiers", testISIDENTIFIER_identifier},
                                 {"Test esc chars", testISIDENTIFIER_EscChars},
                                 {"Test numbers", testISIDENTIFIER_Integers},
                                 CU_TEST_INFO_NULL};

  CU_TestInfo isvalid_tests[] = {{"Test valid identifiers", testISVALID_Valid},
                                 {"Test esc chars", testISVALID_Esc},
                                 {"Test invalid", testISVALID_Invalid},
                                 CU_TEST_INFO_NULL};

  CU_TestInfo strconcat_tests[] = {{"Test first string", testSTRCONCAT_First},
                                   {"Test esc chars", testSTRCONCAT_Second},
                                   {"Test invalid", testSTRCONCAT_Third},
                                   CU_TEST_INFO_NULL};
 
  CU_SuiteInfo suites[] = {{"is_digit testing", init_suite1, clean_suite1,
                           isdigit_tests},
                           {"is_alpha testing", init_suite1, clean_suite1,
                           isalpha_tests},
                           {"is_space testing", init_suite1, clean_suite1,
                           isspace_tests},
                           {"is_identifier testing", init_suite1, clean_suite1,
                           isidentifier_tests},
                           {"is_valid testing", init_suite1, clean_suite1,
                           isvalid_tests},
                           {"str_concat testing", init_suite1, clean_suite1,
                           strconcat_tests},
                           CU_SUITE_INFO_NULL};

  /* initialize the CUnit test registry */
  if (CUE_SUCCESS != CU_initialize_registry())
    return CU_get_error();

  if (CU_register_suites(suites)) {
    CU_cleanup_registry();
    return CU_get_error();
  }

  CU_basic_set_mode(CU_BRM_VERBOSE);
  CU_basic_run_tests();
  CU_cleanup_registry();
  return CU_get_error();
}
