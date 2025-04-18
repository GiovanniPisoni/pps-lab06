package ex2

import ex2.ConferenceReviewing.*

import java.util
import ex1.List
import List.*
/**
 * An interface modelling the results of reviewing articles of a conference
 * Each reviewer (revisore) reads an article (articolo), and answers to a number of questions
 * with a score from 0 (bad) to 10 (excellent).
 * Note that each article can be reviewed by many reviewers (typically, from 2 to 4), but the
 * system does not keep track of the identity of reviewers
 *
 */
object ConferenceReviewing:

/**
 * For each article, the reviewer has to reply to all the following questions
 */
  enum Question:
    case RELEVANCE // ("È importante per questa conferenza?"),
    case SIGNIFICANCE // ("Produce contributo scientifico?"),
    case CONFIDENCE // ("Ti senti competente a commentarlo?");
    case FINAL  // ("É un articolo da accettare?")

end ConferenceReviewing

trait ConferenceReviewing:
  /**
   * @param article the article to be reviewed
   * @param scores a map from questions to scores
   * loads a review for the specified article, with complete scores as a map
   */
  def loadReview(article: Int, scores: util.Map[Question, Int]): Unit

  /**
   * @param article the article to be reviewed
   * @param relevance the score for the RELEVANCE question
   * @param significance the score for the SIGNIFICANCE question
   * @param confidence the score for the CONFIDENCE question
   * @param fin the score for the FINAL question
   * loads a review for the specified article, with the 4 explicit scores
   */
  def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit

  /**
   * @param article the article to be reviewed
   * @param question the question to be considered
   * @return the scores given to the specified article and specified question, as an (ascending-ordered) list
   */
  def orderedScores(article: Int, question: Question): util.List[Int]

  /**
   * @param article the article to be reviewed
   * @return the average score to question FINAL taken by the specified article
   */
  def averageFinalScore(article: Int): Double

  /**
   * An article is considered accept if its averageFinalScore (not weighted) is > 5,
   * and at least one RELEVANCE score that is >= 8.
   *
   * @return the set of accepted articles
   */
  def acceptedArticles: util.Set[Int]


  /**
   * @return accepted articles as a list of pairs article+averageFinalScore, ordered from worst to best based on averageFinalScore
   */
  def sortedAcceptedArticles: util.List[(Int, Double)]

  /**
   * @return a map from articles to their average "weighted final score", namely,
   *         the average value of CONFIDENCE*FINAL/10
   *         Note: this method is optional in this exam
   */
  def averageWeightedFinalScoreMap: util.Map[Int, Double]

class ConferenceReviewingImpl extends ConferenceReviewing:

  private val reviewsList: util.List[(Int, util.Map[Question, Int])] = util.ArrayList[(Int, util.Map[Question, Int])]()

  override def loadReview(article: Int, scores: util.Map[Question, Int]): Unit =
    if scores.size() < Question.values.length then
      throw new IllegalArgumentException("Not all questions have been answered")
    else reviewsList.add((article, scores))

  override def loadReview(article: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
    val map = new util.HashMap[Question, Int]()
    map.put(Question.RELEVANCE, relevance)
    map.put(Question.SIGNIFICANCE, significance)
    map.put(Question.CONFIDENCE, confidence)
    map.put(Question.FINAL, fin)
    this.loadReview(article, map)

  override def orderedScores(article: Int, question: Question): util.List[Int] =
    reviewsList.stream()
      .filter(_._1 == article)
      .map(_._2.get(question))
      .sorted()
      .collect(util.stream.Collectors.toList())

  override def averageFinalScore(article: Int): Double =
    reviewsList.stream()
      .filter(_._1 == article)
      .mapToDouble(_._2.get(Question.FINAL))
      .average()
      .orElse(0.0)

  override def acceptedArticles: util.Set[Int] =
    reviewsList.stream()
      .map(_._1)
      .filter(this.averageFinalScore(_) > 5.0)
      .filter(article =>
        reviewsList.stream()
          .filter(_._1 == article)
          .map(_._2.entrySet())
          .flatMap(_.stream())
          .anyMatch(x => x.getKey == Question.RELEVANCE && x.getValue >= 8))
      .collect(util.stream.Collectors.toSet())

  override def sortedAcceptedArticles: util.List[(Int, Double)] =
    this.acceptedArticles.stream()
      .map(article => (article, this.averageFinalScore(article)))
      .sorted((e1,e2) => e1._2.compareTo(e2._2))
      .collect(util.stream.Collectors.toList())

  private def _averageWeightedFinalScore(article: Int): Double =
    reviewsList.stream()
      .filter(_._1 == article)
      .mapToDouble(review => review._2.get(Question.FINAL) * review._2.get(Question.CONFIDENCE) / 10.0)
      .average()
      .orElse(0.0)

  override def averageWeightedFinalScoreMap: util.Map[Int, Double] =
    reviewsList.stream()
      .map(_._1)
      .distinct()
      .collect(util.stream.Collectors.toMap[Int, Int, Double](
        article => article, _averageWeightedFinalScore _
      ))

end ConferenceReviewingImpl

/**
 * Si consulti la documentazione dell'interfaccia ConferenceReviewing, che modella i risultati del processo di revisione
 * degli articoli di una conferenza. Ogni articolo viene da revisionato da uno o più revisori anonimi, ognuno dei quali fornisce
 * una valutazione (score) da 0 a 10 per 4 diverse "domande", modellate da ConferenceReviewing.Question. Un articolo viene
 * accettato se il valore medio della valutazione alla domanda "FINAL" è >5 e se ha almeno una valutazione "RELEVANCE" >= 8.
 *
 * Implementare ConferenceReviewing attraverso una classe ConferenceReviewingImpl con costruttore senza argomenti,
 * in modo che passi tutti i test di cui sotto, realizzati per essere autoesplicativi.
 *
 * Sono considerati opzionali ai fini della possibilità di correggere l'esercizio, ma concorrono comunque al raggiungimento
 * della totalità del punteggio:
 * - implementazione dei test opzionali (relativi alla realizzazione del metodo averageWeightedFinalScoreMap)
 * - la qualità della soluzione, in particolare con minimizzazione di ripetizioni e codice non inutilmente complesso
 */
class ConferenceReviewingTest:
  import org.junit.*
  import org.junit.Assert.*
  import org.junit.Before

  private var cr: ConferenceReviewing = ConferenceReviewingImpl()

  @Before def init(): Unit =
    this.cr = new ConferenceReviewingImpl
    // carico una revisione per l'articolo 1:
    // - 8 per relevance, significance e final
    // - 7 per confidence
    // si ricordi che l'ordine delle domande è: relevance, significance, confidence, final
    this.cr.loadReview(1, 8, 8, 6, 8) // 4.8 è il voto finale pesato (usato da averageWeightedFinalScoreMap)
    // simile per gli altri
    this.cr.loadReview(1, 9, 9, 6, 9) // 5.4
    this.cr.loadReview(2, 9, 9, 10, 9) // 9.0
    this.cr.loadReview(2, 4, 6, 10, 6) // 6.0
    this.cr.loadReview(3, 3, 3, 3, 3) // 0.9
    this.cr.loadReview(3, 4, 4, 4, 4) // 1.6
    this.cr.loadReview(4, 6, 6, 6, 6) // 3.6
    this.cr.loadReview(4, 7, 7, 8, 7) // 5.6
    val map = new util.HashMap[Question, Int]()
    map.put(Question.RELEVANCE, 8)
    map.put(Question.SIGNIFICANCE, 8)
    map.put(Question.CONFIDENCE, 7) // 5.6
    map.put(Question.FINAL, 8)
    this.cr.loadReview(4, map)
    this.cr.loadReview(5, 6, 6, 6, 10) // 6.0
    this.cr.loadReview(5, 7, 7, 7, 10) //

  @Test def testOrderedScores(): Unit =
    // l'articolo 2 ha preso su RELEVANCE i due voti 4,9
    assertEquals(cr.orderedScores(2, Question.RELEVANCE), util.Arrays.asList(4, 9))
    // e simile per gli altri
    assertEquals(cr.orderedScores(4, Question.CONFIDENCE), util.Arrays.asList(6, 7, 8))
    assertEquals(cr.orderedScores(5, Question.FINAL), util.Arrays.asList(10, 10))

  @Test def testAcceptedArticles(): Unit =
    // solo gli articoli 1,2,4 vanno accettati, avendo media finale >=5 e almeno un voto su RELEVANCE >= 8
    assertEquals(cr.acceptedArticles, new util.HashSet(util.Arrays.asList(1, 2, 4)))


  @Test def testSortedAcceptedArticles(): Unit =
    // articoli accettati, e loro voto finale medio
    assertEquals(cr.sortedAcceptedArticles, util.Arrays.asList((4, 7.0), (2, 7.5), (1, 8.5)))


  @Test def optionalTestAverageWeightedFinalScore(): Unit =
    // l'articolo 1 ha media pesata finale pari a (4.8+5.4)/2 = 5,1, con scarto massimo 0.01
    assertEquals(cr.averageWeightedFinalScoreMap.get(1), (4.8 + 5.4) / 2, 0.01)
    // e simile per gli altri
    assertEquals(cr.averageWeightedFinalScoreMap.get(2), (9.0 + 6.0) / 2, 0.01)
    assertEquals(cr.averageWeightedFinalScoreMap.get(3), (0.9 + 1.6) / 2, 0.01)
    assertEquals(cr.averageWeightedFinalScoreMap.get(4), (3.6 + 5.6 + 5.6) / 3, 0.01)
    assertEquals(cr.averageWeightedFinalScoreMap.get(5), (6.0 + 7.0) / 2, 0.01)
    assertEquals(cr.averageWeightedFinalScoreMap.size, 5)

end ConferenceReviewingTest